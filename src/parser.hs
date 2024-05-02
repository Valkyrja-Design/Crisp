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
spaceConsumer = L.space space1 (L.skipLineComment "//") (L.skipBlockCommentNested "/*" "*/")

-- Automatically consume trailing spaces after a lexeme
lexeme = L.lexeme spaceConsumer

-- Expression enclosed in ()
parenthesizedExpr :: Parser a -> Parser a 
parenthesizedExpr p = try $ do 
                        _ <- char '('
                        spaceConsumer
                        val <- p 
                        _ <- char ')'
                        return val

-- Expr of the form 'expr
quoted :: Parser a -> Parser a 
quoted x = try (char '\'') *> x

specialIdentifiers :: Parser Text
specialIdentifiers = string "-" 
                <|> string "++"
                <|> string "+"
                <|> string "*"
                <|> string "/" 
                <|> string "<=" 
                <|> string "<"
                <|> string ">=" 
                <|> string ">" 
                <|> string "!="
                <|> string "=="
                <|> string "&&"
                <|> string "||"
                
-- Identifiers start with a letter and then zero or more of alphanumeric
identifier :: Parser Text
identifier = try (do 
                    start <- letterChar 
                    rest <- many (alphaNumChar <|> char '_')
                    return (singleton start <> T.pack rest)
                ) <|> specialIdentifiers

-- Parse integers
signedInteger :: Parser Integer
signedInteger = try $ L.signed (return ()) L.decimal

-- Parse empty list
nil :: Parser ()
nil = try (char '\'' *> string "()") *> return () <?> "nil"

-- Parse booleans
boolean :: Parser CrispVal 
boolean = string "false" $> Bool False
        <|> string "true" $> Bool True

-- Parse string literals
stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

-- Parse CrispVals
crispVal :: Parser CrispVal 
crispVal = lexeme $ boolean 
        <|> Nil <$ nil 
        <|> Number <$> signedInteger
        <|> Atom <$> identifier
        <|> String . T.pack <$> stringLiteral
        <|> quoted1 <$> quoted crispVal
        <|> List <$> parenthesizedExpr manyCrispVal

-- Parse quoted val
quoted1 :: CrispVal -> CrispVal
quoted1 x = List [Atom "quote", x]

-- Parse multiple CrispVals separated by whitespace
manyCrispVal :: Parser [CrispVal]
manyCrispVal = crispVal `sepBy` spaceConsumer

contents :: Parser a -> Parser a 
contents x = spaceConsumer *> lexeme x <* eof 

-- read code from string
readCode :: T.Text -> Either (ParseErrorBundle T.Text Void) CrispVal
readCode = parse (contents crispVal) "<stdin>"

-- read code from file
readCodeFromFile :: String -> T.Text -> Either (ParseErrorBundle T.Text Void) CrispVal
readCodeFromFile = parse (contents (List <$> manyCrispVal))
