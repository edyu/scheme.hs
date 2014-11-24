module Main where

import Control.Monad
import Data.Array
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Vector (Array Int LispVal)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                        "#t" -> Bool True
                        "#f" -> Bool False
                        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

spaces :: Parser ()
spaces = skipMany1 space

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseVector :: Parser LispVal
parseVector = do xs <- sepBy parseExpr spaces
                 return $ Vector (listArray (0, (length xs - 1)) xs)

parseExpr :: Parser LispVal
parseExpr = parseString
         <|> parseNumber
         <|> parseQuoted
         <|> try (do string "#("
                     x <- parseVector
                     char ')'
                     return x)
         <|> parseAtom
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))
