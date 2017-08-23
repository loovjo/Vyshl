module Parser where

import Data.Char
import Control.Applicative

data Parser a = Parser { parse :: String -> Maybe (a, String)}

instance Functor Parser where
    fmap f parser =
        Parser $ \x ->
            case parse parser x of
                Just (a, rest) -> Just (f a, rest)
                Nothing -> Nothing

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f =
    Parser $ \x ->
        case parse p x of
            Just (a, rest) -> parse (f a) rest
            Nothing -> Nothing

instance Applicative Parser where
    pure a = 
        Parser $ \x -> 
            Just (a, x)
    p1 <*> p2 = 
        Parser $ \x ->
            case parse p2 x of
                Just (a, rest) ->
                    case parse p1 rest of
                        Just (f, rest') -> Just (f a, rest') 
                        Nothing -> Nothing
                Nothing -> Nothing

instance Monad Parser where
    return = pure
    (>>=) = bind

instance Alternative Parser where
    p1 <|> p2 = 
        Parser $ \x ->
            case parse p1 x of
                Just (a, rst) -> Just (a, rst)
                Nothing -> parse p2 x
    empty =
        Parser $ const Nothing

    many p =
        Parser $ \x ->
            case parse p x of
                Just (a, rest) ->
                    case parse (many p) rest of
                        Just (as, rest') -> Just (a : as, rest')
                        Nothing -> Just ([a], rest)
                Nothing -> Just ([], x)

    some p =
        Parser $ \x ->
            case parse p x of
                Just (a, rest) ->
                    case parse (many p) rest of
                        Just (as, rest') -> Just (a : as, rest')
                        Nothing -> Just ([a], rest)
                Nothing -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = 
    Parser $ \x ->
        case x of
            a:xs ->
                if f a
                    then Just (a, xs)
                    else Nothing
            [] -> Nothing

anyOf :: [Parser a] -> Parser a
anyOf [] = empty
anyOf (p:ps) = p <|> anyOf ps

char :: Char -> Parser Char
char c =
    Parser $ \x ->
        case x of
            a:xs -> 
                if a == c 
                    then Just (a, xs)
                    else Nothing
            [] -> Nothing

-- Only parse this very string, fail if it's not present
string :: String -> Parser String
string [] = return []
string s@(c:cs) = do
    char c
    string cs
    return s

token :: Parser x -> Parser x
token p = do
    s <- p
    spaces
    return s

reserved = token . string

spaces :: Parser String
spaces = many $ satisfy $ flip elem $ " \n\r"

parens :: Parser x -> Parser x
parens p = do
    reserved "("
    x <- p
    reserved ")"
    return x

alterparens :: Parser x -> Parser x
alterparens x = x <|> parens (alterparens x)

int :: (Read a, Integral a) => Parser a
int = do
    negative <- string "-" <|> return []
    num <- some $ satisfy isDigit
    return $ read $ negative ++ num

float :: (Floating a, Read a) => Parser a
float = (do
        wholePart <- int
        char '.'
        rest <- (some $ satisfy isDigit) <|> return "0"
        return $ read $ show wholePart ++ "." ++ rest
    ) <|> (do
        wholePart <- int
        return $ fromIntegral wholePart
    )
