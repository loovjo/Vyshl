{-# LANGUAGE ViewPatterns, TupleSections #-}
module Types where

import Data.Maybe
import Base
import Builtins
import Running


tMatch :: Type -> Type -> Bool
tMatch T_Any _ = True
tMatch _ T_Any = True
tMatch (T_Func a1 b1) (T_Func a2 b2) = a1 `tMatch` a2 && b1 `tMatch` b2
tMatch x y     = x == y

-- Gets the 'higher' of two types, where higher means which one contains the most information.
-- Eg. Between a T_Any and a T_Bool, T_Bool contains more information
higher :: Type -> Type -> Either TypeError Type
higher x y
    | x `tMatch` y =
        case x of
            T_Any -> Right y
            T_Func f1 a1 ->
                case y of
                    T_Func f2 a2 -> do
                            f <- f1 `higher` f2
                            a <- a1 `higher` a2
                            return $ T_Func f a
                    T_Any -> return x
            _ -> Right x
    | otherwise = Left $ TypeMismatch x y

toT_Func :: Type -> Either TypeError Type
toT_Func f@(T_Func _ _) = Right f
toT_Func T_Any = Right $ T_Func T_Any T_Any
toT_Func x = Left $ NotFunction x


data TypeError
    = TypeMismatch Type Type
    | NotFunction Type
    deriving (Show, Eq)

type TypeContext = [(String, Type)]

getFromContext :: TypeContext -> String -> Type
getFromContext [] _ = T_Any
getFromContext ((v, t):rest) var
    | var == v  = t
    | otherwise = getFromContext rest var
    
putToContext :: TypeContext -> (String, Type) -> TypeContext
putToContext [] var = [var]
putToContext ctx@((v, vType) : rest) var@(name, newType) = 
    if v /= name
        then (v, vType) : putToContext rest var
        else
            case vType `higher` newType of
                Right combined -> (v, combined) : rest
                _ -> var : rest

defaultTypeContext =
    mapMaybe (\x ->
        case x of
            (name, A_Builtin _ t _) -> Just (name, t)
            _ -> Nothing
    ) defaultContext

typeCheck :: Ast -> Type -> TypeContext -> Either TypeError (Type, TypeContext)

typeCheck (A_Num _) t ctx = (, ctx) <$> t `higher` T_Num

typeCheck (A_Bool _) t ctx = (, ctx) <$> t `higher` T_Bool

typeCheck (A_If cond true false) t ctx = do
    (tCond, ctx1) <- typeCheck cond T_Bool ctx
    (tTrue, ctx2) <- typeCheck true t ctx1
    (tFalse, ctx3) <- typeCheck false tTrue ctx2
    (tTrue', ctx4) <- typeCheck true tFalse ctx3 -- Needed for things like `if x then y else 0`, without, y would be :: *

    (, ctx4) <$> tTrue' `higher` tFalse


typeCheck (A_Variable v) t1 ctx@(flip getFromContext v -> t2) =
    (\res -> (res, putToContext ctx (v, res))) <$> t1 `higher` t2

typeCheck (A_Builtin label t f) t' ctx = (, ctx) <$> t `higher` t'

typeCheck (A_App f a) t ctx = do
    (tF, ctx1) <- typeCheck f (T_Func T_Any T_Any) ctx
    case tF of
        T_Func arg res -> do
            (tA, ctx2) <- typeCheck a arg ctx1

            return (res, ctx2)
        _ ->
            Left $ TypeMismatch tF (T_Func T_Any T_Any)

typeCheck (A_Lambda var body) t ctx = do
    (T_Func arg res) <- t `higher` T_Func T_Any T_Any
    (tBody, ctx1) <- typeCheck body res ctx
    (tVar, ctx2)  <- typeCheck var arg ctx1

    return $ (T_Func tVar tBody, ctx)

{-
getType :: Ast -> TypeContext -> Either TypeError Type

getType (A_Num _) _ = Right T_Num

getType (A_Bool _) _ = Right T_Bool

getType (A_Variable v) ctx = getFromContext ctx v

getType x@(A_BinOp _ _ _) ctx =
    getType (reduce x defaultContext) ctx

getType (A_If cond true false) ctx = do
    tCond  <- getType cond ctx
    case tCond of
        T_Bool -> do
            tTrue  <- getType true ctx
            tFalse <- getType false ctx

            if tTrue == tFalse
                then return tTrue
                else Left $ TypeMismatch tTrue tFalse
        _ ->
            Left $ TypeMismatch tCond T_Bool


-}
