{-# LANGUAGE OverloadedStrings #-}

module Parser (
    readCode,
    readCodeFromFile
) where
    
import CrispVal ( CrispVal(List, Bool, Nil, Number, String, Atom) )

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void 
import Data.Text as T
import Data.Functor
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad

type Parser = Parsec Void Text

-- Consumes whitespace and comments
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "--") (L.skipBlockCommentNested "{-" "-}")

-- Automatically consume trailing spaces after a lexeme
lexeme = L.lexeme spaceConsumer

-- Expression enclosed in ()
parenthesizedExpr :: Parser a -> Parser a 
parenthesizedExpr p = try $ do 
                        _ <- char '('
                        val <- p 
                        _ <- char ')'
                        return val

-- Expr of the form 'expr
quoted :: Parser a -> Parser a 
quoted x = try (char '\'') *> x

specialIdentifiers :: Parser Text
specialIdentifiers = string "-" <|> string "+" <|> string "..."

-- Identifiers start with a letter and then zero or more of alphanumeric
identifier :: Parser Text
identifier = try (do 
                    start <- letterChar 
                    rest <- many (alphaNumChar <|> char '_')
                    return (singleton start <> T.pack rest)
                ) <|> specialIdentifiers

-- Parse integers
signedInteger = try $ L.signed (return ()) L.decimal

-- Parse empty list
nil :: Parser ()
nil = try (char '\'' *> string "()") *> return () <?> "nil"

-- Parse booleans
boolean :: Parser CrispVal 
boolean = string "False" $> Bool False
        <|> string "True" $> Bool True

-- Parse string literals
stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

-- Parse CrispVals
crispVal :: Parser CrispVal 
crispVal = boolean 
        <|> Nil <$ nil 
        <|> Number <$> signedInteger
        <|> Atom <$> identifier
        <|> String <$> T.pack <$> stringLiteral
        <|> quoted1 <$> quoted crispVal
        <|> List <$> parenthesizedExpr manyCrispVal

-- Parse quoted val
quoted1 :: CrispVal -> CrispVal
quoted1 x = List [Atom "quote", x]

-- Parse multiple CrispVals separated by whitespace
manyCrispVal :: Parser [CrispVal]
manyCrispVal = crispVal `sepBy` space1

contents :: Parser a -> Parser a 
contents x = space *> lexeme x <* eof 

-- read code from string
readCode = parse (contents crispVal) "<stdin>"

-- read code from file
readCodeFromFile = parse (contents (List <$> manyCrispVal))
