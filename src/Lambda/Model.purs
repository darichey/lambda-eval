module Lambda.Model (Term(..)) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Term = Var String | Abs String Term | App Term Term

derive instance Eq Term

derive instance Generic Term _

instance Show Term where
  show x = genericShow x
