{- resolution: A resolution-based theorem prover in Haskell -}

module Main where
import System.Environment
import Parser

toCNF :: Term -> Term
toCNF (Dis (Var a) (x `Con` y)) = Con (toCNF $ Dis (Var a) x) (toCNF $ Dis (Var a) y)
toCNF (Dis (x `Con` y) (Var a)) = toCNF $ Dis (Var a) (x `Con` y)
toCNF oth = oth

main = do
    (path:_) <- getArgs
    contents <- readFile path
    -- Why do this fancy mapM_ stuff here?
    -- Because main has type IO () (for this function).
    -- Therefore, we have to ensure that our last expression returns IO ().
    -- We want to do a bunch of stuff to a list and not care about the results,
    -- which is precisely what mapM_ does. (cf. mapM, which would return IO
    -- [()].
    mapM_ (print . toCNF) $ readLines contents
