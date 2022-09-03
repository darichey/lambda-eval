module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array.NonEmpty (toArray)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Parsing (Parser, fail, runParser)
import Parsing.Combinators (between, chainl1)
import Parsing.Combinators.Array (many1)
import Parsing.String (char)
import Parsing.String.Basic (alphaNum, skipSpaces, space)

main :: Effect Unit
main = do
  case runParser "(\\x.x) x" term of
    Left e -> logShow e
    Right t -> logShow $ reduce t

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

varString :: Parser String String
varString = fromCharArray <$> toArray <$> many1 alphaNum

var :: Parser String Term
var = Var <$> varString

abs :: Parser String Term -> Parser String Term
abs p = Abs <$> (char '\\' *> skipSpaces *> varString <* skipSpaces) <*> (char '.' *> skipSpaces *> p)

nonApp :: Parser String Term -> Parser String Term
nonApp p = (between (char '(') (char ')') p) <|> abs p <|> var

term :: Parser String Term
term = fix \p -> skipSpaces *> chainl1 (nonApp p) (space *> pure App)
