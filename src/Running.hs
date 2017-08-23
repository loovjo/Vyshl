
module Running where

import Base

data Context
    = Empty
    | Var (String, Ast) Context


getFromContext :: String -> Context -> Maybe Ast
getFromContext _ Empty = Nothing
getFromContext x (Var (x', y) ctx)
    | x == x' = Just y
    | otherwise = getFromContext x ctx


reduce :: Ast -> Context -> Ast

reduce (A_Num x) _ = A_Num x

reduce (A_BinOp op x y) ctx = A_App (A_App (reduce op ctx) (reduce x ctx)) (reduce y ctx)
reduce (A_Variable x) ctx =
    case getFromContext x ctx of
        Just y -> y
        Nothing -> A_Variable x

reduce (A_App (A_Lambda (A_Variable var) app) x) ctx =
    reduce app $ Var (var, x) ctx

reduce a@(A_App (A_App (A_Variable op) (A_Num x)) (A_Num y)) _ = 
    let res =
            case op of
                "+" -> Just $ x + y
                "-" -> Just $ x - y
                "*" -> Just $ x * y
                "/" -> Just $ x / y
                "**" -> Just $ x ** y
                _ -> Nothing
    in case res of
        Just x -> A_Num x
        Nothing -> a

reduce (A_App f x) ctx = A_App (reduce f ctx) (reduce x ctx)

reduce x _ = x
