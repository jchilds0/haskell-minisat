module MiniSat.Solver (Model(..), Literal(..), Variable(..), Constr, Sign(..), solve, notVariable) where

data Sign = None | Not
    deriving Show

flipSign :: Sign -> Sign
flipSign None = Not
flipSign Not = None

applySign :: Bool -> Sign -> Bool
applySign val None = val
applySign val Not = not val

data Variable a = Bound a Sign Bool | Unbound a Sign
    deriving Show

variableName :: Variable a -> a
variableName (Bound name _ _) = name
variableName (Unbound name _) = name

variableSign :: Variable a -> Sign
variableSign (Bound _ sign _) = sign
variableSign (Unbound _ sign) = sign

variableValue :: Variable a -> Bool
variableValue (Bound _ sign val) = applySign val sign
variableValue (Unbound _ _) = False

instance Eq a => Eq (Variable a) where
    var1 == var2 = variableName var1 == variableName var2

notVariable :: Variable a -> Variable a
notVariable (Bound name sign bool) = Bound name (flipSign sign) bool
notVariable (Unbound name sign) = Unbound name (flipSign sign) 

isBound :: Variable a -> Bool
isBound (Bound _ _ _) = True
isBound (Unbound _ _) = False

type Constr a = [Variable a]

data Literal a = Literal a Bool
    deriving (Eq, Show, Ord)

data Model a = Model [Constr a] [Variable a] [Literal a]
    deriving Show

type SAT = Bool

solve :: Eq a => Model a -> Maybe [Literal a]
solve model = if sat then Just assigns else Nothing
    where
        (result, sat) = solveVar model
        Model _ _ assigns = result

solveVar :: Eq a => Model a -> (Model a, SAT)
solveVar (Model cs ls as) = case ls of
    (var:_) -> if sat1 then (modelTrue, sat1) else (modelFalse, sat2)
        where
            model = Model cs ls as
            name = variableName var

            lTrue = Literal name True
            (modelTrue, sat1) = assignVariable model lTrue

            lFalse = Literal name False
            (modelFalse, sat2) = assignVariable model lFalse

    [] -> (Model cs ls as, modelSat cs)

assignVariable :: Eq a => Model a -> Literal a -> (Model a, SAT)
assignVariable model assign
  | conflict1 = (newModel, False)
  | conflict2 = (propModel, False)
  | otherwise = (solveModel, sat)
  where
      newModel = updateVariable model assign
      (Model cs1 _ _) = newModel
      conflict1 = any constrConflict cs1

      propModel = propagateVariable newModel
      (Model cs2 _ _) = propModel
      conflict2 = any constrConflict cs2

      (solveModel, sat) = solveVar propModel

constrConflict :: Constr a -> Bool
constrConflict cs = all isBound cs && not (constrSat cs)

propagateVariable :: Eq a => Model a -> Model a
propagateVariable model = if null unit then model else propagateVariable newModel
    where
        (Model cs _ _) = model
        unit = filter unitConstr cs

        var = unBoundVariable (head unit)
        sign = variableSign var
        assign = Literal (variableName var) (applySign True sign)
        newModel = updateVariable model assign

unBoundVariable :: Constr a -> Variable a
unBoundVariable = head . filter (not . isBound)

countUnBound :: Variable a -> Int -> Int
countUnBound lit count = if isBound lit then count else count + 1

unitConstr :: Constr a -> Bool
unitConstr constr = numUnBound == 1 && not (any variableValue constr)
    where
        numUnBound = foldr countUnBound 0 constr

updateVariable :: Eq a => Model a -> Literal a -> Model a
updateVariable (Model cs ls as) assign = Model newConstrs newVariables newLiterals
    where
        newConstrs = map (map (replaceVariable assign)) cs
        newVariables = filter (not . sameVariable assign) ls
        newLiterals = assign:as

replaceVariable :: Eq a => Literal a -> Variable a -> Variable a
replaceVariable literal var 
    | sameVariable literal var = Bound name sign val
    | otherwise = var
    where 
        (Literal name val) = literal
        sign = variableSign var

sameVariable :: Eq a => Literal a -> Variable a -> Bool
sameVariable (Literal name _) var = name == variableName var

constrSat :: Constr a -> Bool
constrSat cs = bound && sat
    where
        bound = all isBound cs 
        sat = foldr constrPartialSat False cs

constrPartialSat :: Variable a -> Bool -> Bool
constrPartialSat (Bound _ sign val) b = applySign val sign || b
constrPartialSat (Unbound _ _) b = b

modelSat :: [Constr a] -> Bool
modelSat = all constrSat
