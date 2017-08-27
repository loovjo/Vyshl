
module Running (Context(..), reduce) where

import Base

type Context = [(String, Ast)]


getFromContext :: String -> Context -> Maybe Ast
getFromContext _ [] = Nothing
getFromContext x ((x', y):ctx)
    | x == x' = Just y
    | otherwise = getFromContext x ctx


reduce :: Ast -> Context -> Ast

-- reduce (A_BinOp op x y) ctx = A_App (A_App (reduce op ctx) (reduce x ctx)) (reduce y ctx)
reduce (A_Variable x) ctx =
    case getFromContext x ctx of
        Just y -> y
        Nothing -> A_Variable x

reduce (A_App (A_Lambda (A_Variable var) app) x) ctx =
    reduce app $ (var, x):ctx

reduce (A_App builtin@(A_Builtin _ _ f) x) ctx =
    case f x of
        Just res -> reduce res ctx
        Nothing -> A_App builtin $ reduce x ctx

reduce (A_If (A_Bool b) x y) ctx =
    case b of
        True -> reduce x ctx
        False -> reduce y ctx

reduce (A_App f x) ctx = A_App (reduce f ctx) (reduce x ctx)

reduce x _ = x
