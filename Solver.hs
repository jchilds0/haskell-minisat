module Solver where 
import Data.List (nub)

type LBool = Maybe Bool
type Not = Bool
data Literal = Literal String Not LBool
    deriving Show

notLiteral :: Literal -> Literal
notLiteral (Literal name nt lbool) = Literal name (not nt) lbool

setLiteral :: Literal -> Bool -> Literal
setLiteral (Literal name nt _) b = Literal name nt (Just b)

type Constr = [Literal]

data Assign = Assign String Bool
    deriving (Eq, Show)

data Model = Model [Constr] [Literal]
    deriving Show

newVar :: String -> Not -> Literal
newVar str nt = Literal str nt Nothing

solve :: Model -> Maybe [Assign]
solve m1 = if sat then Just (solAssign m2) else Nothing
    where 
        (m2, sat) = solverVar m1

solverVar :: Model -> (Model, Bool)
solverVar m@(Model cs lits) = case lits of 
    (literal:ls) -> if sat1 then (Model sol1 (lTrue:ls1), sat1) else (Model sol2 (lFalse:ls2), sat2)
        where 
            lTrue = setLiteral literal True
            Model cs1 _ = propagateLiteral m lTrue 
            (Model sol1 ls1, sat1) = solverVar (Model cs1 ls)

            lFalse = setLiteral literal False 
            Model cs2 _ = propagateLiteral m lFalse
            (Model sol2 ls2, sat2) = solverVar (Model cs2 ls)

    [] -> (Model cs lits, modelSat cs)

propagateLiteral :: Model -> Literal -> Model
propagateLiteral (Model cs ls) newLit = Model newConstrs ls
    where 
        newConstrs = map (map (replaceLiteral newLit)) cs

replaceLiteral :: Literal -> Literal -> Literal 
replaceLiteral l1 l2 = if name1 == name2 then Literal name1 nt var else l2
    where 
        Literal name1 _ var = l1 
        Literal name2 nt _ = l2 

literalToAssign :: Literal -> Assign
literalToAssign (Literal name _ (Just var)) = Assign name var

solAssign :: Model -> [Assign]
solAssign (Model _ ls) = nub (map literalToAssign ls)

isBound :: Literal -> Bool
isBound literal = case literal of 
    Literal _ _ (Just _) -> True
    Literal _ _ Nothing  -> False

orLiteral :: Literal -> Bool -> Bool
orLiteral (Literal _ _ Nothing) b = b
orLiteral (Literal _ False (Just x)) b = x || b
orLiteral (Literal _ True (Just x)) b = not x || b

constrSat :: Constr -> Bool
constrSat cs = bound && sat
    where 
        bound = all isBound cs
        sat = foldr orLiteral False cs

modelSat :: [Constr] -> Bool
modelSat = all constrSat
