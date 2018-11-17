{- resolution: A resolution-based theorem prover in Haskell -}

module Main where
import System.Environment
import Parser
import Resolve
import Data.List

a = Var "A"
b = Var "B"
c = Var "C"
d = Var "D"

u = Dis a (Dis b c)
v = Dis (Neg a) (Dis b c)

ul = flattenDis u
vl = flattenDis v

isQuery :: Term -> Bool
isQuery (Query _) = True
isQuery _ = False

main = do
    (path:_) <- getArgs
    statements <- parseFile path
    let (queries, clauses) = partition isQuery statements
        cnfClauses = clausesToCNF clauses
        trueQueries = filter (clausesEntail cnfClauses) queries
    print cnfClauses
    print queries
    putStrLn "Queries that follow:"
    mapM_ print trueQueries
