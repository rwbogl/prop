module Resolve where

import Parser
import Data.List
import Data.Maybe

-- Translate a term into conjunctive normal form.
cnf :: Term -> Term
-- Recursively apply De Morgan's law.
cnf (Dis (Var a) (x `Con` y)) = left `Con` right
    where left = cnf $ Dis (Var a) x
          right = cnf $ Dis (Var a) y
-- Swap argument order to fall into first case.
cnf (Dis (x `Con` y) (Var a)) = cnf $ Dis (Var a) (x `Con` y)
cnf (Query t) = Query $ cnf t
cnf oth = oth

-- Split the top-level conjunctives off into separate lists.
splitCons :: [Term] -> [Term]
splitCons = foldr (\term acc -> splitCon term ++ acc) []
    where splitCon (Con x y) = splitCon x ++ splitCon y
          splitCon other = [other]

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
-- resolve :: Term -> [Term] -> Bool
-- resolve query clauses = (not . sat) $ map cnf (neg query : clauses)

-- Flatten an n-ary disjunction into a list of literals.
flattenDis :: Term -> [Term]
flattenDis (Dis x y) = flattenDis x ++ flattenDis y
flattenDis other = [other]

{-| Given two disjunctions of literals (in list form), try to resolve them in
   any way possible.
-}
resolve :: [Term] -> [Term] -> Maybe [Term]
resolve left right = do
    x <- find (\x -> neg x `elem` right) left
    return $ (left \\ [x]) `union` (right \\ [neg x])
