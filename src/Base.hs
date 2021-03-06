module Base where

import Data.List

data Type
    = T_Num
    | T_Bool
    | T_Any
    | T_Func Type Type
    deriving Eq

instance Show Type where
    show T_Num = "Num"
    show T_Bool = "Bool"
    show T_Any = "*"
    show (T_Func arg res) = "(" ++ show arg ++ " -> " ++ show res ++ ")"

-- I'm so sorry
instance Show (x -> y) where
    show _ = "[function]"

instance Eq (x -> y) where
    _ == _ = True

data Ast
    = A_Variable String
    | A_Num Float
    | A_Bool Bool
--    | A_BinOp Ast Ast Ast
    | A_Let (Ast, Ast) Ast
    | A_If Ast Ast Ast -- If x then a else b
    | A_App Ast Ast
    | A_Lambda Ast Ast
    | A_Builtin String Type (Ast -> Maybe Ast) -- A builtin function, can't be made from parsing.
    deriving (Show, Eq)

prettyShow (A_Num x) = show x
prettyShow (A_Bool x) = show x
prettyShow (A_Variable x) = x
prettyShow (A_Let (var, val) exp) = "let " ++ show var ++ " = " ++ show val ++ " in " ++ show exp
prettyShow (A_If cond x y) = "if " ++ prettyShow cond ++ " then " ++ prettyShow x ++ " else " ++ prettyShow y
prettyShow (A_App f x) = "(" ++ prettyShow f ++ ") (" ++ prettyShow x ++ ")"
-- prettyShow (A_BinOp op a b) = "(" ++ prettyShow a ++ " " ++ prettyShow op ++ " " ++ prettyShow b ++ ")"
prettyShow (A_Lambda v e) = "(\\" ++ prettyShow v ++ " -> " ++ prettyShow e ++ ")"
prettyShow (A_Builtin label t f) = "Function " ++ show label ++ " :: " ++ show t


concatArgs :: Ast -> Maybe (Ast, [Ast]) -- Takes a function such as `((f x) y) z` and returns `Just (f, [x, y, z])`
concatArgs (A_App f arg) =
    case concatArgs f of
        Just (f', args) -> Just (f', args ++ [arg])
        Nothing -> Just (f, [arg])
concatArgs _ = Nothing

diagram :: Ast -> [String]
diagram (A_Num x) = [show x]
diagram (A_Bool x) = [show x]
diagram (A_Variable x) = [x]
diagram (A_Let (var, val) exp) =
    dindent
        ["let"]
        [ dindent
            (diagram var)
            [ diagram val ]
        , diagram exp
        ]
diagram (A_If cond x y) =
    dindent
        ["if"]
        [ dindent ["then"] [diagram x]
        , dindent ["else"] [diagram y]
        ]
-- diagram x@(A_App _ _) =
--     let Just (f, args) = concatArgs x
--     in
--         dindent
--             ["Apply"]
--             $ map diagram $ f : args
--
diagram (A_App f x) =
    dindent
        ["App"]
        [ diagram f
        , diagram x ]

-- diagram (A_BinOp op a b) =
--     diagram $ A_App (A_App op a) b

diagram (A_Lambda arg app) =
    dindent
        (map ("\\ " ++) $ diagram arg)
        [ diagram app ]

diagram f@(A_Builtin _ _ _) = [show f]

dindent :: [String] -> [[String]] -> [String]
dindent title [] = title
dindent title blocks =
    let initBlocks =
            map (\x ->
                let firstline = "|- " ++ head x
                    other = map ("|  " ++) $ tail x
                in firstline : other
            ) $ init blocks
        lastBlock = last blocks
        iLastBlock =
            let lFirst = "\\- " ++ head lastBlock
                lOther = map ("   " ++) $ tail lastBlock
            in lFirst : lOther
    in
       title ++ concat initBlocks ++ iLastBlock
