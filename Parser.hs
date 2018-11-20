module Parser where

import qualified Text.Parsec.Token as Token
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec
import Data.List

{-|
   How this can be improved:

        1. We're explicitly parsing by line. If we want to do the
        period-separator idea, then we'll need to nix that. To nix that, we'll
        need to actually understand how to use Parsec.

-}

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

expr = buildExpressionParser table term

table = [ [Prefix (reservedOp "~" >> return Neg)]
        , [Infix (reservedOp "*" >> return Con) AssocRight]
        , [Infix (reservedOp "+" >> return Dis) AssocRight]
        , [Prefix (reservedOp "?" >> return Query)]
        ]

term = parens expr <|> fmap Var identifier

lexer = Token.makeTokenParser def
identifier = Token.identifier lexer
reservedOp = Token.reservedOp lexer
whitespace = Token.whiteSpace lexer
parens = Token.parens lexer

prophParse = whitespace >> declarations

declarations = endBy1 expr (char '.' >> whitespace)

{-| Parse a file into a list of terms. -}
parseFile :: String -> IO [Term]
parseFile file = do
    contents <- readFile file
    return $ case parse prophParse "" contents of
      Left err -> [Var $ "parse error: " ++ show err]
      Right val -> val
