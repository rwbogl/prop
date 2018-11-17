module Resolve where

import Parser
import Data.List
import Data.Maybe

-- This represents a sequence of disjunctions of literals joined with
-- conjunctions.
type DisList = [Term]
type CNF = [DisList]

-- Negate a term.
neg :: Term -> Term
neg (Var a) = Neg (Var a)
neg (Dis x y) = Con (neg x) (neg y)
neg (Con x y) = Dis (neg x) (neg y)
neg (Query x) = Query $ neg x
neg (Neg t) = t

clausesEntail :: CNF -> Term -> Bool
clausesEntail clauses (Query x) = not . sat $ negated ++ clauses
    where negated = clauseToCNF $ neg x

{-| Translate a list of terms into a CNF list. -}
clausesToCNF :: [Term] -> CNF
clausesToCNF = map flattenDis . splitCons . map cnf

clauseToCNF :: Term -> CNF
clauseToCNF = clausesToCNF . (: [])

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

sat :: CNF -> Bool
sat clauses = sat' clauses []

{- Resolution algorithm:
    1. If possible, choose any two clauses with complementary literals. If
       impossible, return True.
    2. Form the resolvant.
    3. If it's empty, then return false.
    4. If it's not empty, then go back to step 1.
-}

sat' :: CNF -> [(DisList, DisList)] -> Bool
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
resolve :: DisList -> DisList -> Maybe DisList
resolve left right = do
    x <- find (\x -> neg x `elem` right) left
    return $ (left \\ [x]) `union` (right \\ [neg x])
