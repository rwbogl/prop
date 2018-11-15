{- resolution: A resolution-based theorem prover in Haskell -}

module Main where
import System.Environment
import Parser
import Resolve

-- I'm not sure what the linter is complaining about here. Something about
-- functors? I don't really know what those are yet.
-- (I think that it's just a way to map and raise back to the monad. Something
-- like `f <$> g == g >>= (return . f)`.
-- Oh yeah! Monads support fmap, which is really what we're doing here. <$> is
-- just an infix synonym for fmap.
parseFile :: String -> IO [Term]
parseFile path = fmap (toCNF . readLines) $ readFile path
    where toCNF = splitCons . map cnf

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
