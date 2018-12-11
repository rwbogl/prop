module Resolve
    ( clausesEntail
    , sat
    , Proof
    , ProofStep
    , QueueItem(left, right, res)
    ) where

import Parser
import Data.List
import Data.Maybe
import Logic

type Proof = Queue
type ProofStep = QueueItem

{-| Check if a CNF list of clauses entail a query. -}
clausesEntail :: CNF -> Term -> Bool
clausesEntail clauses (Query x) = isJust . sat $ negated ++ clauses
    where negated = clauseToCNF $ neg x

data QueueItem = QueueItem { left :: DisList
                           , right :: DisList
                           , res :: DisList
                           , prev :: QueueItem
                           }
               | Empty
               deriving Show

type Queue = [QueueItem]

{-| Unwrap a QueueItem via its previous pointers into a Queue.
-}
queueToList :: QueueItem -> Queue
queueToList head = build head []
    where build Empty acc = acc
          build head acc = build (prev head) (head : acc)

{-| Try to prove that a CNF list of clauses is unsatisfiable. If so, then
   return the proof as a QueueItem. Otherwise, return Nothing. -}
sat :: CNF -> Maybe Queue
sat clauses =
    sat' clauses [Empty] [] >>= return . queueToList

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
