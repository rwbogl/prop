module Resolve where

import Parser
import Data.List
import Data.Maybe

-- Negate a term.
neg :: Term -> Term
neg (Var a) = Neg (Var a)
neg (Dis x y) = Con (neg x) (neg y)
neg (Con x y) = Dis (neg x) (neg y)
neg (Query x) = Query $ neg x
neg (Neg t) = t

{-
   Can `query` be proven from a given list of clauses?
   To check:
        1. Append the negation of `query` to the list.
        2. Apply the resolution algorithm to the resulting list.
        3. If a contradiction occurs, then our query must be true.
        4. If the clauses are satisfiable, then we cannot prove the query from
        the clauses.
-}


{-| Given two disjunctions of literals (in list form), try to resolve them in
   any way possible.
-}
resolve :: [Term] -> [Term] -> Maybe [Term]
resolve left right = do
    x <- find (\x -> neg x `elem` right) left
    return $ (left \\ [x]) `union` (right \\ [neg x])
