module Language where

import Control.Applicative
import Data.Char

import Base
import Parser

binOps = ["+", "-", "*", "/", "**", "==", "!="]
keywords = ["if", "then", "else", "let", "in"]

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
    letStmt <|>
    smallTerm

smallTerm =
    atom <|>
    parens term <|>
    (pFail (const "Need a token!"))

atom =
    num <|> bool <|> var

num = fmap A_Num (token float)

var = (do
        first <- satisfy (\x -> isAlpha x || x == '_' || ord x > 0xFFFF && generalCategory x /= PrivateUse)
        rest <- token $ many $ satisfy (\x -> isAlphaNum x || x == '_' || ord x > 0xFFFF && generalCategory x /= PrivateUse)
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

binOpSmall :: Parser Ast
binOpSmall = 
    fmap A_Variable (pReduce longest $ map reserved binOps)
    <|> (do
        reserved "`"
        func <- smallTerm
        reserved "`"
        return $ func
    )

ifOp = do
    reserved "if"
    cond <- term
    reserved "then"
    true <- term
    reserved "else"
    false <- term
    return $ A_If cond true false

lambda = alterparens $ 
    (do
        char '\\'
        var <- smallTerm
        reserved "->"
        app <- term
        return $ A_Lambda var app
    ) <|>
    (do
        char 'λ'
        var <- smallTerm
        reserved "."
        app <- term
        return $ A_Lambda var app
    )

letStmt = do
    reserved "let"

    vars <- letVars

    reserved "in"

    exp <- term

    return $ foldr A_Let exp vars

letVars :: Parser [(Ast, Ast)]
letVars = 
    (do
        var <- letVar
        reserved ";"
        rest <- letVars
        return $ var : rest
    ) <|> (do
        var <- letVar
        return $ [var]
    )

letVar = do
    var <- term
    reserved "="
    exp <- term
    return (var, exp)

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
