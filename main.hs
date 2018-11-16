{- resolution: A resolution-based theorem prover in Haskell -}

module Main where
import System.Environment
import Parser
import Resolve

{-| Parse a file into CNF form where each sublist contains the literal
   arguments of a disjunction.

   Examples:
        (A + B) * (C + D) -> [[A, B], [C, D]]
        (A * B) + C -> [[A, C], [B, C]]
-}
parseFile :: String -> IO [[Term]]
parseFile path = fmap (toCNF . readLines) $ readFile path
    where toCNF = map flattenDis . splitCons . map cnf

main = do
    (path:_) <- getArgs
    putStrLn $ "Conjunctive normal form of " ++ path ++ ":"
    -- Why do this fancy mapM_ stuff here?
    -- Because main has type IO () (for this function).
    -- Therefore, we have to ensure that our last expression returns IO ().
    -- We want to do a bunch of stuff to a list and not care about the results,
    -- which is precisely what mapM_ does. (cf. mapM, which would return IO
    -- [()].
    parseFile path >>= mapM_ print
