module Lambda.Pretty (prettyPrint) where

import Prelude

import Lambda.Model (Term(..))

prettyPrint :: Term -> String
prettyPrint = go 0
  where
  go :: Int -> Term -> String
  go outerPrec = case _ of
    Var v -> v
    Abs v e -> showParen (outerPrec > 0) $ "\\" <> v <> ". " <> go 0 e
    App e1 e2 -> showParen (outerPrec > 1) $ go 1 e1 <> " " <> go 2 e2

  showParen :: Boolean -> String -> String
  showParen b p = if b then "(" <> p <> ")" else p
