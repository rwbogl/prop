module Parser
    ( ThrowsError
    , parseInput
    ) where

import qualified Text.Parsec.Token as Token
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec
import Data.List
import Control.Monad.Except
import Logic

type ThrowsError = Either ParseError

{-|
   How this can be improved:

        1. We're explicitly parsing by line. If we want to do the
        period-separator idea, then we'll need to nix that. To nix that, we'll
        need to actually understand how to use Parsec.

-}

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

term = parens expr <|> fmap Var identifier <?> "beginning of expression"

lexer = Token.makeTokenParser def
identifier = Token.identifier lexer
reservedOp = Token.reservedOp lexer
whitespace = Token.whiteSpace lexer
parens = Token.parens lexer

propParse = whitespace >> declarations

declarations = endBy1 expr (char '.' >> whitespace) <?> "declaration"

{-| Parse a file into a list of terms. -}
parseInput :: String -> ThrowsError [Term]
parseInput = parse propParse ""
