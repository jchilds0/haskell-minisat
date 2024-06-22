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

stringToClause :: Int -> Variable Int
stringToClause var 
    | var < 0 = Variable (abs var) Not
    | var > 0 = Variable var None
    | otherwise = error $ "var " ++ show var ++ " is invalid"

dimacsToModel :: [String] -> Model Int 
dimacsToModel ls = newModel constrs vars
    where 
        (constrs, vars) = stringToClauses ls

stringToClauses :: [String] -> ([[Variable Int]], [Variable Int])
stringToClauses [] = ([], [])
stringToClauses (line:lines') = case lineType line of
    Constraint -> (constr:cs, ls) 
    Problem -> (cs, lits)
    _ -> (cs, ls)
    where 
        (cs, ls) = stringToClauses lines'
        constr = map stringToClause vars
        vars = filter (/= 0)  (map read ws)
        ws = words line

        lits = map (\n -> Variable n None) [1..varCount]
        varCount = read (words line !! 2) :: Int
