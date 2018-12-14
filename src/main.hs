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

handleStatements :: [Term] -> IO ()
handleStatements statements = do
    let (queries, clauses) = partition isQuery statements
    mapM_ (clausesEntailProof clauses) queries

main = do
    (path:_) <- getArgs
    contents <- readFile path
    case parseInput contents of
      Left err -> print err
      Right statements -> handleStatements statements
