module Language where

import Control.Applicative
import Data.Char

import Base
import Parser

binOps = ["+", "-", "*", "/", "**", "=="]

langParse :: String -> Either String Ast
langParse code =
    case parse term code of
        Just (a, []) -> Right a
        Just (a, b) -> Left $ "Chars didn't get parsed: '" ++ b ++ "'"
        Nothing -> Left "Coundn't parse!"

term :: Parser Ast
term =
    binOp <|>
    ifOp <|>
    func <|>
    lambda <|>
    smallTerm

smallTerm =
    atom <|>
    parens term

atom :: Parser Ast
atom =
    fmap A_Num (token float) <|> var

var :: Parser Ast
var = (do
        first <- satisfy isAlpha
        rest <- token $ many $ satisfy isAlphaNum
        return $ A_Variable $ first : rest
    ) <|> parens binOpSmall

binOp :: Parser Ast
binOp = do
    a <- smallTerm
    op <- binOpSmall
    b <- term
    return $ A_BinOp op a b

binOpSmall :: Parser Ast
binOpSmall = fmap A_Variable $ anyOf $ map reserved binOps

ifOp :: Parser Ast
ifOp = do
    reserved "if"
    cond <- term
    reserved "then"
    true <- term
    reserved "else"
    false <- term
    return $ A_If cond true false

lambda :: Parser Ast
lambda = alterparens $ do
    reserved "\\"
    var <- smallTerm
    reserved "->"
    app <- term
    return $ A_Lambda var app

func :: Parser Ast
func = do
    f <- smallTerm
    x <- term
    return $ A_App f x
