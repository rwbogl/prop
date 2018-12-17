{- prop - A simple resolution-based theorem prover for propositional logic. -}

module Main where
import System.Environment
import Parser
import Resolve
import GenProof
import Logic
import Data.List
import Options.Applicative
import Data.Semigroup ((<>))

isQuery :: Term -> Bool
isQuery (Query _) = True
isQuery _ = False

data Arguments = Arguments
    { quiet :: Bool
    , path :: String
    }

argParse :: Parser Arguments
argParse = Arguments
        <$> switch
            ( long "quiet"
           <> short 'q'
           <> help "Suppress output"
            )
        <*> argument str
            ( metavar "PATH"
           <> help "Proposition source file queries"
            )

handleStatements :: [Term] -> IO ()
handleStatements statements = do
    let (queries, clauses) = partition isQuery statements
    mapM_ (clausesEntailProof clauses) queries

handleArgs :: Arguments -> IO ()
handleArgs args = do
    contents <- readFile $ path args
    case parseInput contents of
      Left err -> print err
      Right statements -> handleStatements statements

main :: IO ()
main = handleArgs =<< execParser opts
    where opts = info (argParse <**> helper)
                  ( fullDesc
                 <> progDesc "Read a propositional file and output query results."
                 <> header "prop - a resolution-based theorem prover"
                  )
