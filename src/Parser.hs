module Parser where

import Data.Char
import Control.Applicative

data Parser a = Parser { parse :: String -> Either (String, Int) (a, String)}

instance Functor Parser where
    fmap f parser =
        Parser $ \x ->
            case parse parser x of
                Right (a, rest) -> Right (f a, rest)
                Left err -> Left err

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f =
    Parser $ \x ->
        case parse p x of
            Right (a, rest) -> parse (f a) rest
            Left err -> Left err

instance Applicative Parser where
    pure a =
        Parser $ \x -> 
            Right (a, x)
    p1 <*> p2 = 
        Parser $ \x ->
            case parse p2 x of
                Right (a, rest) ->
                    case parse p1 rest of
                        Right (f, rest') -> Right (f a, rest') 
                        Left err -> Left err
                Left err -> Left err

instance Monad Parser where
    return = pure
    (>>=) = bind

instance Alternative Parser where
    p1 <|> p2 = 
        Parser $ \x ->
            case parse p1 x of
                Right res -> Right res
                Left err -> parse p2 x
    empty =
        Parser $ \x -> Left $ ("Empty!", length x)

    many p =
        Parser $ \x ->
            case parse p x of
                Right (a, rest) ->
                    case parse (many p) rest of
                        Right (as, rest') -> Right (a : as, rest')
                        Left _ -> Right ([a], rest)
                Left _ -> Right ([], x)

    some p =
        Parser $ \x ->
            case parse p x of
                Right (a, rest) ->
                    case parse (many p) rest of
                        Right (as, rest') -> Right (a : as, rest')
                        Left _ -> Right ([a], rest)
                Left err -> Left err

pFail :: (String -> String) -> Parser x
pFail x =
    Parser $ \s -> Left (x s, length s)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = 
    Parser $ \x ->
        case x of
            a:xs ->
                if f a
                    then Right (a, xs)
                    else Left $ ("Couldn't satisfy function on char " ++ show a, length x)
            [] -> Left ("Empty stream!", length x)

anyOf :: [Parser a] -> Parser a
anyOf = pReduce (<|>)

pReduce :: (Parser a -> Parser a -> Parser a) -> [Parser a] -> Parser a
pReduce f (p:ps) = p `f` pReduce f ps
pReduce _ [] = empty

longest :: Parser a -> Parser a -> Parser a
longest p1 p2 = Parser $ \x ->
    case parse p1 x of
        Left _ -> parse p2 x
        Right (res1, left1) ->
            case parse p2 x of
                Left _ -> Right (res1, left1)
                Right (res2, left2) ->
                    if length left1 > length left2
                        then Right (res2, left2)
                        else Right (res1, left1)

char :: Char -> Parser Char
char c =
    Parser $ \x ->
        case x of
            a:xs -> 
                if a == c 
                    then Right (a, xs)
                    else Left ("Couldn't find char " ++ show c, length x)
            [] -> Left ("End of stream!", length x)

-- Only parse this very string, fail if it's not present
string :: String -> Parser String
string [] = return []
string s@(c:cs) = 
    (do
        char c
        string cs
        return s
    ) <|>
        ( pFail (\x -> "Looking for " ++ show s ++ " but found " ++ show x) )

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
