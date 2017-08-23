module Base where

data Ast
    = A_BinOp Ast Ast Ast
    | A_Variable String
    | A_Num Float
    | A_If Ast Ast Ast -- If x then a else b
    | A_App Ast Ast
    | A_Lambda Ast Ast
    | A_Assign Ast Ast
    deriving (Show, Eq)

prettyShow (A_Num x) = show x
prettyShow (A_Variable x) = x
prettyShow (A_If cond x y) = "if " ++ prettyShow cond ++ " then " ++ prettyShow x ++ " else " ++ prettyShow y
prettyShow (A_App f x) = prettyShow f ++ " " ++ prettyShow x
prettyShow (A_BinOp op a b) = "(" ++ prettyShow a ++ " " ++ prettyShow op ++ " " ++ prettyShow b ++ ")"
prettyShow (A_Lambda v e) = "(\\" ++ prettyShow v ++ " -> " ++ prettyShow e ++ ")"


concatArgs :: Ast -> Maybe (Ast, [Ast]) -- Takes a function such as `((f x) y) z` and returns `Just (f, [x, y, z])`
concatArgs (A_App f arg) =
    case concatArgs f of
        Just (f', args) -> Just (f', args ++ [arg])
        Nothing -> Just (f, [arg])
concatArgs _ = Nothing

diagram :: Ast -> [String]
diagram (A_Num x) = [show x]
diagram (A_Variable x) = [x]
diagram (A_If cond x y) =
    dindent
        ["if"]
        [ dindent ["then"] [diagram x]
        , dindent ["else"] [diagram y]
        ]
diagram x@(A_App _ _) =
    let Just (f, args) = concatArgs x
    in
        dindent
            ("Apply")
            $ map diagram $ f : args

diagram (A_BinOp op a b) =
    diagram $ A_App (A_App op a) b

diagram (A_Lambda arg app) =
    dindent
        (map ("\\ " ++) $ diagram arg)
        [ diagram app ]

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
