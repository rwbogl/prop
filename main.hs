{- prop - A simple resolution-based theorem prover for propositional logic. -}

module Main where
import System.Environment
import Parser
import Resolve
import GenProof
import Logic
import Data.List

isQuery :: Term -> Bool
isQuery (Query _) = True
isQuery _ = False

{-|
    Okay, things look pretty good so far!

    Next up:

        Improve the data structure design. I have plans to implement a few
        different proof algorithms, and the current design is awful. There are
        `DisList`s and `CNF`s floating around everywhere.

        I think that some of this could be cleared up by using `newtype` rather
        than `type` to define the `DisList` and `CNF` types, but I might have
        to experiment a bit and find out.
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
