module MiniSat.Parser where 

import MiniSat.Solver

data RowType = Comment | Problem | Constraint | Other

lineType :: String -> RowType
lineType line 
    | length w <= 1 = Other
    | head w == "c" = Comment
    | head w == "p" = Problem
    | last w == "0" = Constraint
    | otherwise = Other
    where 
        w = words line

stringToClause :: Int -> Literal
stringToClause var 
    | var < 0 = Literal (show (abs var)) True Nothing 
    | var > 0 = Literal (show var) False Nothing 
    | otherwise = error $ "var " ++ show var ++ " is invalid"

dimacsToModel :: [String] -> Model
dimacsToModel [] = Model [] [] []
dimacsToModel (line:lines') = case lineType line of
    Constraint -> Model (clause:cs) ls []
    Problem -> Model cs lits []
    _ -> model
    where 
        model = dimacsToModel lines'
        Model cs ls _ = model
        clause = map stringToClause vars
        vars = filter (/= 0)  (map read ws)
        ws = words line

        lits = map (\n -> Literal (show n) False Nothing) [1..varCount]
        varCount = read (words line !! 2) :: Int
