module MiniSat.Solver (Model(..), Assign(..), Literal(..), Constr, solve, newVar, notLiteral) where

type LBool = Maybe Bool
type Not = Bool
data Literal a = Literal a Not LBool
    deriving Show

instance Eq a => Eq (Literal a) where
    Literal name1 _ _ == Literal name2 _ _ = name1 == name2

notLiteral :: Literal a -> Literal a
notLiteral (Literal name nt lbool) = Literal name (not nt) lbool

value :: Literal a -> Bool
value (Literal _ _ Nothing) = False
value (Literal _ False (Just var)) = var
value (Literal _ True (Just var)) = not var

isBound :: Literal a -> Bool
isBound literal = case literal of
    Literal _ _ (Just _) -> True
    Literal _ _ Nothing  -> False

type Constr a = [Literal a]

data Assign a = Assign a Bool
    deriving (Eq, Show, Ord)

data Model a = Model [Constr a] [Literal a] [Assign a]
    deriving Show

newVar :: a -> Not -> Literal a
newVar str nt = Literal str nt Nothing

type SAT = Bool

solve :: Eq a => Model a -> Maybe [Assign a]
solve model = if sat then Just assigns else Nothing
    where
        (result, sat) = solveVar model
        Model _ _ assigns = result

solveVar :: Eq a => Model a -> (Model a, SAT)
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

assignLiteral :: Eq a => Model a -> Assign a -> (Model a, SAT)
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

constrConflict :: Constr a -> Bool
constrConflict cs = all isBound cs && not (constrSat cs)

propagateLiteral :: Eq a => Model a -> Model a
propagateLiteral model = if null unit then model else propagateLiteral newModel
    where
        (Model cs _ _) = model
        unit = filter unitConstr cs

        (Literal name nt _) = unBoundLiteral (head unit)
        assign = Assign name (not nt)
        newModel = updateLiteral model assign

unBoundLiteral :: Constr a -> Literal a
unBoundLiteral = head . filter (not . isBound)

countUnBound :: Literal a -> Int -> Int
countUnBound lit count = if isBound lit then count else count + 1

unitConstr :: Constr a -> Bool
unitConstr constr = numUnBound == 1 && not (any value constr)
    where
        numUnBound = foldr countUnBound 0 constr

updateLiteral :: Eq a => Model a -> Assign a -> Model a
updateLiteral (Model cs ls as) assign = Model newConstrs newLiterals newAssigns
    where
        newConstrs = map (map (replaceLiteral assign)) cs
        newLiterals = filter (not . sameLiteral assign) ls
        newAssigns = assign:as

sameLiteral :: Eq a => Assign a -> Literal a -> Bool
sameLiteral (Assign name1 _) (Literal name2 _ _) = name1 == name2

replaceLiteral :: Eq a => Assign a -> Literal a -> Literal a
replaceLiteral assign literal
    | sameLiteral assign literal = Literal name nt (Just var) 
    | otherwise = literal
    where
        Assign _ var = assign
        Literal name nt _ = literal

constrSat :: Constr a -> Bool
constrSat cs = bound && sat
    where
        bound = all isBound cs 
        sat = foldr (\x y -> y || value x) False cs

modelSat :: [Constr a] -> Bool
modelSat = all constrSat
