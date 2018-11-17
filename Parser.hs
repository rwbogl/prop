module Parser where

import qualified Text.Parsec.Token as Token
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec
import Data.List

data Term = Var String
          | Dis Term Term
          | Con Term Term
          | Neg Term
          | Query Term
          deriving Eq

instance Show Term where show = showTerm

showTerm (Var s) = s
showTerm (Dis x y) = "(" ++ show x ++ ") + (" ++ show y ++ ")"
showTerm (Con x y) = "(" ++ show x ++ ") * (" ++ show y ++ ")"
showTerm (Neg x) = "~(" ++ show x ++ ")"
showTerm (Query x) = "?(" ++ show x ++ ")"

def = emptyDef { Token.commentStart = "/*"
               , Token.commentEnd = "*/"
               , Token.identStart = letter
               , Token.identLetter = alphaNum
               , Token.reservedOpNames = ["+", "~", "*"]
               }

parseExpr = buildExpressionParser table term

table = [ [Prefix (reservedOp "~" >> return Neg)]
        , [Infix (reservedOp "*" >> return Con) AssocRight]
        , [Infix (reservedOp "+" >> return Dis) AssocRight]
        , [Prefix (reservedOp "?" >> return Query)]
        ]

term = parens parseExpr <|> fmap Var identifier

lexer = Token.makeTokenParser def
identifier = Token.identifier lexer
reservedOp = Token.reservedOp lexer
whitespace = Token.whiteSpace lexer
parens = Token.parens lexer

readExpr :: String -> Term
readExpr input =
    case parse parseExpr "" input of
      Left err -> Var (show err)
      Right val -> val

readLines :: String -> [Term]
readLines = map readExpr . lines

{-| Parse a file into a list of terms. -}
parseFile :: String -> IO [Term]
parseFile = fmap readLines . readFile
