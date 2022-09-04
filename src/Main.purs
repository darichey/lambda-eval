module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array.NonEmpty (toArray)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Lambda.Parse (term)
import Lambda.Pretty (prettyPrint)
import Parsing (runParser)

main :: Effect Unit
main = do
  case runParser "(\\f. \\x. f x) (\\y. y) (\\z. z)" term of
    Left e -> logShow e
    Right t -> log $ prettyPrint t
