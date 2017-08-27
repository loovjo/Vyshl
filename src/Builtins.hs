module Builtins (defaultContext, biNumFunc) where

import Base


defaultContext =
    [ ("abs", A_Builtin "abs" (T_Func T_Num T_Num) f_abs)
    , ("+", biNumFunc "add" (+))
    , ("-", biNumFunc "sub" (-))
    , ("*", biNumFunc "mul" (*))
    , ("/", biNumFunc "div" (/))
    ]

biNumType = (T_Func T_Num (T_Func T_Num T_Num))

biNumFunc :: String -> (Float -> Float -> Float) -> Ast
biNumFunc name f =
    A_Builtin name biNumType $ \v ->
        case v of
            A_Num x ->
                Just $
                    A_Builtin ("half " ++ name ++ " " ++ show x) (T_Func T_Num T_Num) $ \ y' ->
                        case y' of
                            A_Num y -> Just $ A_Num $ f x y
                            _ -> Nothing
            _ -> Nothing

f_abs :: Ast -> Maybe Ast
f_abs (A_Num x) = Just $ A_Num $ abs x
f_abs x = Nothing
