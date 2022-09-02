module Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class.Console (logShow)

main :: Effect Unit
main = do
  logShow $ reduce term
    where
    term = App (Abs "x" (Var "x")) (Abs "y" (Var "y"))

data Term = Var String | Abs String Term | App Term Term

derive instance Generic Term _

instance Show Term where
  show x = genericShow x

cas :: Term -> Term -> String -> Term
cas (Var v1) e v2 = if v1 == v2 then e else (Var v1)
cas (Abs v1 e1) e v2 = if v1 == v2 then (Abs v1 e1) else (Abs v1 (cas e1 e v2))
cas (App e1 e2) e v = (App (cas e1 e v) (cas e2 e v))

reduce :: Term -> Term
reduce (App (Abs v e1) e2) = cas e1 e2 v
reduce (App e1 e2) = App e1' e2
  where
  e1' = reduce e1
reduce x = x
