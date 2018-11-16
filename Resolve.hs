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
   We check by contradiction:
        1. Append the negation of `query` to the clauses.
        2. Apply some CNF-SAT solver to the resulting clauses.
        3. If the clauses are unsatisfiable, then our query must be true.
        4. If the clauses are satisfiable, then the query is independent of our
        initial clauses.
checkQuery (Query q) clauses = (not . sat) $ flattenCNF (neg q:clauses)
-}

sat :: [[Term]] -> Bool
sat clauses = sat' clauses []

{- Resolution algorithm:
    1. If possible, choose any two clauses with complementary literals. If
       impossible, return True.
    2. Form the resolvant.
    3. If it's empty, then return false.
    4. If it's not empty, then go back to step 1.
-}

sat' :: [[Term]] -> [([Term], [Term])] -> Bool
sat' clauses seen
  | null resolvants = True
  | any null resolvants = False
  | otherwise = sat' (clauses ++ resolvants) (seen ++ newSeen)
    where resolvePairs = [((x, y), fromJust r) | x <- clauses, y <- clauses,
                                                    (x, y) `notElem` seen,
                                                    let r = resolve x y,
                                                    isJust r]
          (newSeen, resolvants) = unzip resolvePairs

{-| Given two disjunctions of literals (in list form), try to resolve them in
   any way possible.
-}
resolve :: [Term] -> [Term] -> Maybe [Term]
resolve left right = do
    x <- find (\x -> neg x `elem` right) left
    return $ (left \\ [x]) `union` (right \\ [neg x])
