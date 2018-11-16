{- resolution: A resolution-based theorem prover in Haskell -}

module Main where
import System.Environment
import Parser
import Resolve

a = Var "A"
b = Var "B"
c = Var "C"
d = Var "D"

u = Dis a (Dis b c)
v = Dis (Neg a) (Dis b c)

ul = flattenDis u
vl = flattenDis v

main = do
    (path:_) <- getArgs
    putStrLn $ "Conjunctive normal form of " ++ path ++ ":"
    -- Why do this fancy mapM_ stuff here?
    -- Because main has type IO () (for this function).
    -- Therefore, we have to ensure that our last expression returns IO ().
    -- We want to do a bunch of stuff to a list and not care about the results,
    -- which is precisely what mapM_ does. (cf. mapM, which would return IO
    -- [()].
    parseFiletoCNF path >>= mapM_ print
