module Lambda.Reduce (reduce) where

import Prelude

import Lambda.Model (Term(..))

cas :: Term -> Term -> String -> Term
cas (Var v1) e v2 = if v1 == v2 then e else (Var v1)
cas (Abs v1 e1) e v2 = if v1 == v2 then (Abs v1 e1) else (Abs v1 (cas e1 e v2))
cas (App e1 e2) e v = (App (cas e1 e v) (cas e2 e v))

reduce1 :: Term -> Term
reduce1 (App (Abs v e1) e2) = cas e1 e2 v
reduce1 (App e1 e2) = App e1' e2
  where
  e1' = reduce1 e1
reduce1 x = x

reduce :: Term -> Term
reduce x = go x (reduce1 x)
  where
  go prev cur | prev == cur = cur
  go _ cur = go cur (reduce1 cur)
