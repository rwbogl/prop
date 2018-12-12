module Resolve
    ( clausesEntail
    , sat
    , Proof
    , ProofStep(left, right, res)
    ) where

import Data.List
import Data.Maybe
import Logic
import Queue

data ProofStep = ProofStep { left :: DisList
                           , right :: DisList
                           , res :: DisList
                           , prev :: ProofStep
                           }
               | Start

type Proof = [ProofStep]

-- Unfold a ProofStep into a Proof by following its parents.
makeProof :: ProofStep -> Proof
makeProof = reverse . makeProof'
    where makeProof' Start = []
          makeProof' step = step : makeProof' (prev step)

{-| Check if a CNF list of clauses entail a query. -}
clausesEntail :: CNF -> Term -> Bool
clausesEntail clauses (Query x) = isJust . sat $ negated ++ clauses
    where negated = clauseToCNF $ neg x

{-| Try to prove that a CNF list of clauses is unsatisfiable. If so, then
   return the proof as a ProofStep. Otherwise, return Nothing. -}
sat :: CNF -> Maybe Proof
sat clauses = sat' clauses queue [] >>= return . makeProof
    where initialOptions = findResolvants clauses
          queue = fromList $ packResolvants initialOptions Start

{- Resolution algorithm:
    1. If possible, choose any two clauses with complementary literals. If
       impossible, return True.
    2. Form the resolvant.
    3. If it's empty, then return false.
    4. If it's not empty, then go back to step 1.
-}

sat' :: CNF -> Queue ProofStep -> [(DisList, DisList)] -> Maybe ProofStep
sat' init queue seen
  | empty queue = Nothing -- Nothing left to resolve.
  | null (res current) = Just current -- Derived the empty clause.
  | otherwise = sat' init queue' seen'
  where Just (current, rest) = dequeue queue
        seen' = (left current, right current):(right current, left current):seen
        -- Add the current resolvant along with every clause derived on this
        -- path.
        currentClauses = res current : path current
        neighbors = findResolvants (init ++ currentClauses)
        newNeighbors = filter ((`notElem` seen) . fst) neighbors
        queue' = enqueue (packResolvants newNeighbors current) rest

-- Return a list of the resolvants added by following (in reverse) the given
-- steps.
path :: ProofStep -> [DisList]
path Start = []
path item = res item : path (prev item)

-- Given the list of resolvant pairs from findResolvants and the previous proof
-- step, pack these into a list of ProofSteps with appropriate parents.
packResolvants :: [((DisList, DisList), [DisList])] -> ProofStep -> [ProofStep]
packResolvants xs prev = concat [[ProofStep x y r prev | r <- rs] | ((x, y), rs) <- xs]

-- Find all possible resolvants in a CNF list as ((left, right), resolvant)
-- tuples.
findResolvants :: CNF -> [((DisList, DisList), [DisList])]
findResolvants clauses = [((x, y), fullResolve x y) | x <- clauses, y <- clauses]

{-| Fully resolve two CNF terms. That is, return a list of all possible
   resolvants. -}
fullResolve :: DisList -> DisList -> [DisList]
fullResolve left right = [resolve left right x | x <- markedLiterals]
    where markedLiterals = filter (\x -> neg x `elem` right) left
          resolve left right x = (left \\ [x]) `union` (right \\ [neg x])
