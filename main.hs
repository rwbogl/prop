{- proph - A simple resolution-based theorem prover for propositional logic. -}

module Main where
import System.Environment
import Parser
import Resolve
import Proof
import Data.List

isQuery :: Term -> Bool
isQuery (Query _) = True
isQuery _ = False

{-|
    Okay, things look pretty good so far!

    Next up:

        2. Improve our resolution algorithm implementation. Right now we just
        try every possible combination, which is easy to write, but terribly
        inefficient. We should do a proper search algorithm.

        Note: After finishing 3, I think it's cool to find the shortest
        possible proof. We could look at more efficient SAT algorithms, but
        they won't find the shortest one. We'll need to stress test the current
        algorithm and see when it becomes unbearably slow.

        4. Implement a REPL.

    Done:

        1. Improved parser and error handling.

        3. Implemented a simple BFS proof search. This stores the steps and
        also finds the *shortest* proof, if one exists. There's also a proof
        printer now.
-}

handleStatements :: [Term] -> IO ()
handleStatements statements = do
    let (queries, clauses) = partition isQuery statements
        cnfClauses = clausesToCNF clauses
    mapM_ (clausesEntailProof clauses) queries

main = do
    (path:_) <- getArgs
    contents <- readFile path
    case parseInput contents of
      Left err -> print err
      Right statements -> handleStatements statements
