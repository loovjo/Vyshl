module Language where

import Control.Applicative
import Data.Char

import Base
import Parser

binOps = ["+", "-", "*", "/", "**", "==", "!="]
keywords = ["if", "then", "else"]

langParse :: String -> Either String Ast
langParse code =
    case parse mainParser code of
        Right (a, []) -> Right a
        Left (err, left) -> Left $
            code ++ "\n" ++
            replicate left ' ' ++
            "^ " ++ err

mainParser :: Parser Ast
mainParser =
    Parser $ \x ->
        case parse term x of
            Right (a, []) -> Right (a, [])
            Right (a, b) -> Left ("Chars left that didn't get parsed!", length x - length b)
            Left x -> Left x

term =
    binOp <|>
    ifOp <|>
    func <|>
    lambda <|>
    smallTerm

smallTerm =
    atom <|>
    parens term <|>
    (pFail (const "Need a token!"))

atom =
    num <|> bool <|> var

num = fmap A_Num (token float)

var = (do
        first <- satisfy isAlpha
        rest <- token $ many $ satisfy isAlphaNum
        let var = first : rest

        if var `elem` keywords
            then pFail ("Is a keyword: " ++)
            else return $ A_Variable $ first : rest
    ) <|> parens binOpSmall

bool = 
    (reserved "True" >> (return $ A_Bool True))
    <|>
    (reserved "False" >> (return $ A_Bool False))

binOp = do
    a <- smallTerm
    op <- binOpSmall
    b <- term
    return $ A_App (A_App op a) b

binOpSmall = fmap A_Variable $ pReduce longest $ map reserved binOps

ifOp = do
    reserved "if"
    cond <- term
    reserved "then"
    true <- term
    reserved "else"
    false <- term
    return $ A_If cond true false

lambda = alterparens $ do
    char '\\' <|> char 'λ'
    var <- smallTerm
    reserved "->"
    app <- term
    return $ A_Lambda var app

func = do
    f <- smallTerm
    case f of
        A_Num _ -> pFail ("Not a function"++)
        A_Bool _ -> pFail ("Not a function"++)
        _ -> do
            x <- term
            return $ 
                insert x f

{- 
 - f, a b -> (f a) b
 - f, (a b) c -> ((f a) b) c
 - f, ((a b) c) d -> (((f a) b) c) d
 -}

insert :: Ast -> Ast -> Ast -- Insert takes a function, `f` and an applicaion such as `(a b) c` and returns `((f a) b) c`
insert app f =
    case app of
        A_App a b ->
            A_App (insert a f) b
        _ -> A_App f app
