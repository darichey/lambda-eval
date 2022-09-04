module Lambda.Parse (term) where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array.NonEmpty (toArray)
import Data.String.CodeUnits (fromCharArray)
import Lambda.Model (Term(..))
import Parsing (Parser)
import Parsing.Combinators (between, chainl1)
import Parsing.Combinators.Array (many1)
import Parsing.String (char)
import Parsing.String.Basic (alphaNum, skipSpaces, space)

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
