module MiniSat.Solver (Model(..), Assign(..), Literal(..), Constr, solve, newVar, notLiteral) where 

type LBool = Maybe Bool
type Not = Bool
data Literal = Literal String Not LBool
    deriving Show

notLiteral :: Literal -> Literal
notLiteral (Literal name nt lbool) = Literal name (not nt) lbool

setLiteral :: Literal -> Bool -> Literal
setLiteral (Literal name nt _) b = Literal name nt (Just b)

value :: Literal -> Bool
value (Literal _ _ Nothing) = False
value (Literal _ False (Just var)) = var
value (Literal _ True (Just var)) = not var

type Constr = [Literal]

data Assign = Assign String Bool
    deriving (Eq, Show)

data Model = Model [Constr] [Literal] [Assign]
    deriving Show

newVar :: String -> Not -> Literal
newVar str nt = Literal str nt Nothing

type SAT = Bool

solve :: Model -> Maybe [Assign]
solve model = if sat then Just assigns else Nothing
    where 
        (result, sat) = solveVar model
        Model _ _ assigns = result

solveVar :: Model -> (Model, SAT)
solveVar (Model cs ls as) = case ls of 
    (literal:lits) -> if sat1 then (modelTrue, sat1) else (modelFalse, sat2)
        where 
            newModel = Model cs lits as

            lTrue = setLiteral literal True
            (modelTrue, sat1) = assignLiteral newModel lTrue

            lFalse= setLiteral literal False
            (modelFalse, sat2) = assignLiteral newModel lFalse 

    [] -> (Model cs ls as, modelSat cs)

assignLiteral :: Model -> Literal -> (Model, SAT)
assignLiteral model literal = (newModel, sat)
    where 
        Literal name _ _ = literal
        a = Assign name (value literal)

        propModel = propagateLiteral model literal
        (Model constrs1 lits1 as, sat) = solveVar propModel
        newModel = Model constrs1 lits1 (a:as) 

propagateLiteral :: Model -> Literal -> Model
propagateLiteral (Model cs ls as) newLit = Model newConstrs ls as
    where 
        newConstrs = map (map (replaceLiteral newLit)) cs

replaceLiteral :: Literal -> Literal -> Literal 
replaceLiteral l1 l2 = if name1 == name2 then Literal name1 nt var else l2
    where 
        Literal name1 _ var = l1 
        Literal name2 nt _ = l2 

isBound :: Literal -> Bool
isBound literal = case literal of 
    Literal _ _ (Just _) -> True
    Literal _ _ Nothing  -> False

constrSat :: Constr -> Bool
constrSat cs = bound && sat
    where 
        bound = all isBound cs
        sat = foldr (\x y -> y || value x) False cs

modelSat :: [Constr] -> Bool
modelSat = all constrSat
