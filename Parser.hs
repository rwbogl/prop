module Parser where

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
