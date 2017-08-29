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

cAdd :: TypeContext -> TypeContext -> TypeContext
cAdd ctx [] = ctx
cAdd ctx (a:rst) = cAdd (putToContext ctx a) rst

ctxSum :: [TypeContext] -> TypeContext
ctxSum = foldr1 cAdd

defaultTypeContext =
    mapMaybe (\x ->
        case x of
            (name, A_Builtin _ t _) -> Just (name, t)
            _ -> Nothing
    ) defaultContext

typeCheck :: Ast -> Type -> TypeContext -> Either TypeError (Type, TypeContext) -- Takes AST, expected type and context and returns type of expression and new values to the context

typeCheck (A_Num _) t ctx = (, []) <$> t `higher` T_Num
typeCheck (A_Bool _) t ctx = (, []) <$> t `higher` T_Bool


typeCheck (A_If cond true false) t ctx = do
    (tCond, ctx1) <- typeCheck cond T_Bool ctx
    (tTrue, ctx2) <- typeCheck true t (ctx `cAdd` ctx1)
    (tFalse, ctx3) <- typeCheck false tTrue (ctx `cAdd` ctx1 `cAdd` ctx2)
    (tTrue', ctx4) <- typeCheck true tFalse (ctx `cAdd` ctx1 `cAdd` ctx2 `cAdd` ctx3) -- Needed for things like `if x then y else 0`, without, y would be :: *

    (, ctxSum [ctx1, ctx2, ctx3, ctx4]) <$> tTrue' `higher` tFalse


typeCheck (A_Variable v) t1 ctx@(flip getFromContext v -> t2) = do
    res <- t1 `higher` t2
    return (res, [(v, res)])

typeCheck (A_Builtin label t f) t' ctx = (, []) <$> t `higher` t'

typeCheck (A_App f a) t ctx = do
    (tF, ctx1) <- typeCheck f (T_Func T_Any T_Any) ctx
    case tF of
        T_Func arg res -> do
            (tA, ctx2) <- typeCheck a arg (ctx `cAdd` ctx1)

            return (res, ctx1 `cAdd` ctx2)
        _ ->
            Left $ TypeMismatch tF (T_Func T_Any T_Any)

typeCheck (A_Lambda var@(A_Variable name) body) t ctx = do
    (T_Func arg res) <- t `higher` T_Func T_Any T_Any
    (tBody, ctx1') <- typeCheck body res ctx
    let ctx1 = filter (\(v,t) -> v /= name) ctx1' -- Remove var from the resulting context
    (tVar, ctx2) <- typeCheck var arg (ctx `cAdd` ctx1)

    return $ (T_Func tVar tBody, ctx1)
