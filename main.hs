module Main where
import System.Environment
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec

data Term = Variable String
          | Disjunction Term Term
          | Conjunction Term Term
          | Negation Term
          | Query Term
          deriving Show

def = emptyDef { Token.commentStart = "/*"
               , Token.commentEnd = "*/"
               , Token.identStart = letter
               , Token.identLetter = alphaNum
               , Token.reservedOpNames = ["+", "~", "*"]
               }

parseExpr :: Parser Term
parseExpr = buildExpressionParser table term

-- This works because type constructors can be curried. I assume that Parsec is
-- treating whatever we spit out here as a function to apply later, which is
-- happily supplied a term later on.
table = [ [Prefix (reservedOp "?" >> return Query)] 
        , [Prefix (reservedOp "~" >> return Negation)] 
        , [Infix (reservedOp "*" >> return Conjunction) AssocRight]
        , [Infix (reservedOp "+" >> return Disjunction) AssocRight]
        ]

term = parens parseExpr <|> fmap Variable identifier

lexer = Token.makeTokenParser def
identifier = Token.identifier lexer
reservedOp = Token.reservedOp lexer
whitespace = Token.whiteSpace lexer
parens = Token.parens lexer

readExpr :: String -> String
readExpr input =
    case parse parseExpr "resolution" input of
      Left err -> show err
      Right val -> show val

readLines = map readExpr . lines

main = do
    (path:_) <- getArgs
    contents <- readFile path
    -- Why do this fancy mapM_ stuff here?
    -- Because main has type IO () (for this function).
    -- Therefore, we have to ensure that our last expression returns IO ().
    -- We want to do a bunch of stuff to a list and not care about the results,
    -- which is precisely what mapM_ does. (cf. mapM, which would return IO
    -- [()].
    mapM_ putStrLn $ readLines contents
