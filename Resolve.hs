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

{-| Check if a CNF list of clauses entail a query. -}
clausesEntail :: CNF -> Term -> Bool
clausesEntail clauses (Query x) = isJust . sat $ negated ++ clauses
    where negated = clauseToCNF $ neg x

{-| Translate a term into a CNF list. -}
clauseToCNF :: Term -> CNF
clauseToCNF = clausesToCNF . (: [])

{-| Translate a list of terms into a CNF list. -}
clausesToCNF :: [Term] -> CNF
clausesToCNF = map flattenDis . splitCons . map cnf

{-| Unfold a DisList into a term. -}
unfoldDisList :: DisList -> Term
unfoldDisList [x] = x
unfoldDisList (x:xs) = Dis x (unfoldDisList xs)

-- Flatten an n-ary disjunction into a list of literals.
flattenDis :: Term -> [Term]
flattenDis (Dis x y) = flattenDis x ++ flattenDis y
flattenDis other = [other]

-- Split the top-level conjunctives off into separate lists.
splitCons :: [Term] -> [Term]
splitCons = foldr (\term acc -> splitCon term ++ acc) []
    where splitCon (Con x y) = splitCon x ++ splitCon y
          splitCon other = [other]

{-| Translate a term into conjunctive normal form. -}
cnf :: Term -> Term
-- Recursively apply De Morgan's law.
cnf (Dis left right) =
    case (left, right) of
      (x `Con` y, right) -> cnf (x `Dis` right) `Con` cnf (y `Dis` right)
      (left, x `Con` y) -> cnf (x `Dis` left) `Con` cnf (y `Dis` left)
      _ -> Dis left right
-- Swap argument order to fall into first case.
cnf (Query t) = Query $ cnf t
cnf oth = oth

data QueueItem = QueueItem { left :: DisList
                           , right :: DisList
                           , res :: DisList
                           , prev :: QueueItem
                           }
               | Empty
               deriving Show

type Queue = [QueueItem]

{-| Try to prove that a CNF list of clauses is unsatisfiable. If so, then
   return the proof as a QueueItem. Otherwise, return Nothing. -}
sat :: CNF -> Maybe QueueItem
sat clauses = sat' clauses [Empty] []

{- Resolution algorithm:
    1. If possible, choose any two clauses with complementary literals. If
       impossible, return True.
    2. Form the resolvant.
    3. If it's empty, then return false.
    4. If it's not empty, then go back to step 1.
-}

sat' :: CNF -> Queue -> [(DisList, DisList)] -> Maybe QueueItem
sat' init [] _ = Nothing
sat' init (Empty:rest) _ = sat' init (packResolvants (findResolvants init) Empty) []
sat' init (head:rest) seen
  | null $ res head = Just head -- Derived the empty clause.
  | otherwise = sat' init queue seen' -- Add to the queue and move on.
  where seen' = (right head, left head):(left head, right head):seen
        newQueries = res head : path head
        neighbors = findResolvants (init ++ newQueries)
        newNeighbors = filter ((`notElem` seen) . fst) neighbors
        enqueue = packResolvants newNeighbors head
        queue = rest ++ enqueue

path :: QueueItem -> [DisList]
path Empty = []
path item = res item : path (prev item)

packResolvants :: [((DisList, DisList), [DisList])] -> QueueItem -> [QueueItem]
packResolvants xs prev = concat [[QueueItem x y r prev | r <- rs] | ((x, y), rs) <- xs]

findResolvants :: CNF -> [((DisList, DisList), [DisList])]
findResolvants clauses = [((x, y), fullResolve x y) | x <- clauses, y <- clauses]

{-| Fully resolve two CNF terms. That is, return a list of all possible
   resolvants. -}
fullResolve :: DisList -> DisList -> [DisList]
fullResolve left right = [resolve left right x | x <- markedLiterals]
    where markedLiterals = filter (\x -> neg x `elem` right) left
          resolve left right x = (left \\ [x]) `union` (right \\ [neg x])
