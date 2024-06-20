module MiniSat.Solver (Model(..), Assign(..), Literal(..), Constr, solve, newVar, notLiteral) where

type LBool = Maybe Bool
type Not = Bool
data Literal = Literal String Not LBool
    deriving Show

instance Eq Literal where
    Literal name1 _ _ == Literal name2 _ _ = name1 == name2

notLiteral :: Literal -> Literal
notLiteral (Literal name nt lbool) = Literal name (not nt) lbool

value :: Literal -> Bool
value (Literal _ _ Nothing) = False
value (Literal _ False (Just var)) = var
value (Literal _ True (Just var)) = not var

isBound :: Literal -> Bool
isBound literal = case literal of
    Literal _ _ (Just _) -> True
    Literal _ _ Nothing  -> False

type Constr = [Literal]

data Assign = Assign String Bool
    deriving (Eq, Show, Ord)

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
    (literal:_) -> if sat1 then (modelTrue, sat1) else (modelFalse, sat2)
        where
            model = Model cs ls as
            (Literal name _ _) = literal

            lTrue = Assign name True
            (modelTrue, sat1) = assignLiteral model lTrue

            lFalse = Assign name False
            (modelFalse, sat2) = assignLiteral model lFalse

    [] -> (Model cs ls as, modelSat cs)

assignLiteral :: Model -> Assign -> (Model, SAT)
assignLiteral model assign
  | conflict1 = (newModel, False)
  | conflict2 = (propModel, False)
  | otherwise = (solveModel, sat)
  where
      newModel = updateLiteral model assign
      (Model cs1 _ _) = newModel
      conflict1 = any constrConflict cs1

      propModel = propagateLiteral newModel
      (Model cs2 _ _) = propModel
      conflict2 = any constrConflict cs2

      (solveModel, sat) = solveVar propModel

constrConflict :: Constr -> Bool
constrConflict cs = all isBound cs && not (constrSat cs)

propagateLiteral :: Model -> Model
propagateLiteral model = if null unit then model else propagateConstr model (head unit)
    where
        (Model cs _ _) = model
        unit = filter unitConstr cs

propagateConstr :: Model -> Constr -> Model
propagateConstr model constr = newModel
    where
        (Literal name nt _) = unBoundLiteral constr
        assign = Assign name (not nt)
        newModel = updateLiteral model assign

unBoundLiteral :: Constr -> Literal
unBoundLiteral = head . filter (not . isBound)

countUnBound :: Literal -> Int -> Int
countUnBound lit count = if isBound lit then count else count + 1

unitConstr :: Constr -> Bool
unitConstr constr = numUnBound == 1 && not (any value constr)
    where
        numUnBound = foldr countUnBound 0 constr

updateLiteral :: Model -> Assign -> Model
updateLiteral (Model cs ls as) assign = Model newConstrs newLiterals newAssigns
    where
        newConstrs = map (map (replaceLiteral assign)) cs
        newLiterals = filter (not . sameLiteral assign) ls
        newAssigns = assign:as

sameLiteral :: Assign -> Literal -> Bool
sameLiteral (Assign name1 _) (Literal name2 _ _) = name1 == name2

replaceLiteral :: Assign -> Literal -> Literal
replaceLiteral assign literal
    | sameLiteral assign literal = Literal name nt (Just var) 
    | otherwise = literal
    where
        Assign _ var = assign
        Literal name nt _ = literal

constrSat :: Constr -> Bool
constrSat cs = bound && sat
    where
        bound = all isBound cs 
        sat = foldr (\x y -> y || value x) False cs

modelSat :: [Constr] -> Bool
modelSat = all constrSat
