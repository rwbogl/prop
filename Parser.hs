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
          | Empty
          deriving Eq

instance Show Term where show = showTerm

showTerm (Var s) = s
showTerm (Dis x y) = "(" ++ show x ++ ") + (" ++ show y ++ ")"
showTerm (Con x y) = "(" ++ show x ++ ") * (" ++ show y ++ ")"
showTerm (Neg x) = "~(" ++ show x ++ ")"
showTerm (Query x) = "?(" ++ show x ++ ")"
showTerm Parser.Empty = "Empty"

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

{-| Parse a file into CNF form where each sublist contains the literal
   arguments of a disjunction.

   Examples:
        (A + B) * (C + D) -> [[A, B], [C, D]]
        (A * B) + C -> [[A, C], [B, C]]
-}
parseFile :: String -> IO [Term]
parseFile = fmap readLines . readFile

-- Flatten an n-ary disjunction into a list of literals.
flattenDis :: Term -> [Term]
flattenDis (Dis x y) = flattenDis x ++ flattenDis y
flattenDis other = [other]

-- Split the top-level conjunctives off into separate lists.
splitCons :: [Term] -> [Term]
splitCons = foldr (\term acc -> splitCon term ++ acc) []
    where splitCon (Con x y) = splitCon x ++ splitCon y
          splitCon other = [other]
