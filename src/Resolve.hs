module Resolve
    ( clausesEntail
    , clausesEntailCNF
    , satisfiable
    , satisfiableCNF
    , sat
    , Proof
    , ProofStep(parents, res)
    ) where

import Data.List
import Data.Maybe
import Logic
import Queue
import qualified Data.Set as Set

data ProofStep = ProofStep { parents :: ClauseSet
                           , res :: DisList
                           , prev :: ProofStep
                           }
               | Start

type Proof = [ProofStep]
type ClauseSet = Set.Set DisList

-- Unfold a ProofStep into a Proof by following its parents.
makeProof :: ProofStep -> Proof
makeProof = reverse . makeProof'
    where makeProof' Start = []
          makeProof' step = step : makeProof' (prev step)

clausesEntail :: [Term] -> Term -> Bool
clausesEntail clauses = clausesEntailCNF (clausesToCNF clauses)

{-| Check if a CNF list of clauses entail a query. -}
clausesEntailCNF :: CNF -> Term -> Bool
clausesEntailCNF clauses (Query x) = isJust . sat $ negated ++ clauses
    where negated = clauseToCNF $ neg x
clausesEntailCNF _ _ = error "Second argument must be a query"

satisfiable :: [Term] -> Bool
satisfiable = satisfiableCNF . clausesToCNF

satisfiableCNF :: CNF -> Bool
satisfiableCNF = isNothing . sat

{-| Try to prove that a CNF list of clauses is unsatisfiable. If so, then
   return the proof as a Proof. Otherwise, return Nothing. -}
sat :: CNF -> Maybe Proof
sat clauses = sat' clauses queue Set.empty >>= return . makeProof
    where initialOptions = findResolvants clauses
          queue = fromList $ packResolvants initialOptions Start

{- Resolution algorithm:
    1. If possible, choose any two clauses with complementary literals. If
       impossible, return True.
    2. Form the resolvant.
    3. If it's empty, then return false.
    4. If it's not empty, then go back to step 1.
-}

sat' :: CNF -> Queue ProofStep -> Set.Set ClauseSet -> Maybe ProofStep
sat' baseClauses queue seen
  | empty queue = Nothing -- Nothing left to resolve.
  | null (res current) = Just current -- Derived the empty clause.
  | otherwise = sat' baseClauses queue' seen'
  where Just (current, rest) = dequeue queue
        seen' = Set.insert (parents current) seen
        -- Add the current resolvant along with every clause derived on this
        -- path.
        currentClauses = res current : path current
        neighbors = findResolvants (baseClauses ++ currentClauses)
        newNeighbors = filter (\(parents, _) -> not $ Set.member parents seen') neighbors
        queue' = enqueue (packResolvants newNeighbors current) rest

-- Return a list of the resolvants added by following (in reverse) the given
-- steps.
path :: ProofStep -> [DisList]
path Start = []
path item = res item : path (prev item)

-- Given the list of resolvant pairs from findResolvants and the previous proof
-- step, pack these into a list of ProofSteps with appropriate parents.
packResolvants :: [(ClauseSet, [DisList])] -> ProofStep -> [ProofStep]
packResolvants xs prev = concat [[ProofStep set r prev | r <- rs] | (set, rs) <- xs]

-- Find all possible resolvants in a CNF list as ((left, right), resolvant)
-- tuples.
findResolvants :: CNF -> [(ClauseSet, [DisList])]
findResolvants clauses = [(Set.fromList [x, y], fullResolve x y) | x <- clauses, y <- clauses]

{-| Fully resolve two CNF terms. That is, return a list of all possible
   resolvants. -}
fullResolve :: DisList -> DisList -> [DisList]
fullResolve left right = [resolve left right x | x <- markedLiterals]
    where markedLiterals = filter (\x -> neg x `elem` right) left
          resolve left right x = (left \\ [x]) `union` (right \\ [neg x])
