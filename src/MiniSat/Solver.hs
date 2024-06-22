module MiniSat.Solver (Model(..), Literal(..), Variable(..), Constr, Sign(..), solve, notVariable) where

data Sign = None | Not
    deriving Show

flipSign :: Sign -> Sign
flipSign None = Not
flipSign Not = None

data Variable a = Variable a Sign
    deriving Show

notVariable :: Variable a -> Variable a
notVariable (Variable name sign) = Variable name (flipSign sign)

eqVariableName :: Eq a => a -> Variable a -> Bool 
eqVariableName n1 (Variable n2 _) = n1 == n2

type Constr a = [Variable a]

data Literal a = Literal a Bool
    deriving (Eq, Show, Ord)

literalTrue :: a -> Sign -> Literal a
literalTrue name None = Literal name True
literalTrue name Not = Literal name False 

variableSat :: Literal a -> Variable a -> Bool
variableSat (Literal _ True) (Variable _ None) = True
variableSat (Literal _ False) (Variable _ None) = False
variableSat (Literal _ True) (Variable _ Not) = False
variableSat (Literal _ False) (Variable _ Not) = True

data Model a = Model [Constr a] [Variable a] [Literal a]
    deriving Show

type SAT = Bool

solve :: Eq a => Model a -> Maybe [Literal a]
solve model = if sat then Just assigns else Nothing
    where
        (result, sat) = solveVar model
        Model _ _ assigns = result

solveVar :: Eq a => Model a -> (Model a, SAT)
solveVar model@(Model _ ls _) = case ls of
    (var:_) -> if sat1 then (modelTrue, sat1) else (modelFalse, sat2)
        where
            (Variable name _) = var

            lTrue = Literal name True
            (modelTrue, sat1) = assignVariable model lTrue

            lFalse = Literal name False
            (modelFalse, sat2) = assignVariable model lFalse

    [] -> (model, not (constrConflict model))

assignVariable :: Eq a => Model a -> Literal a -> (Model a, SAT)
assignVariable model assign
  | conflict1 = (newModel, False)
  | conflict2 = (propModel, False)
  | otherwise = (solveModel, sat)
  where
      newModel = updateVariable model assign
      conflict1 = constrConflict newModel

      propModel = propagateUnitConstr newModel
      conflict2 = constrConflict propModel

      (solveModel, sat) = solveVar propModel

constrConflict :: Model a -> Bool
constrConflict (Model cs _ _) = any null cs

propagateUnitConstr :: Eq a => Model a -> Model a
propagateUnitConstr model 
    | null unit = model 
    | otherwise = propagateUnitConstr newModel
    where
        (Model cs _ _) = model
        unit = filter unitConstr cs

        (Variable name sign) = head (head unit)
        assign = literalTrue name sign
        newModel = updateVariable model assign

unitConstr :: Constr a -> Bool
unitConstr cs = length cs == 1

updateVariable :: Eq a => Model a -> Literal a -> Model a
updateVariable (Model cs ls as) literal = Model newConstrs newVariables newLiterals
    where
        Literal name _ = literal
        unsatConstrs = filter (unsatLiteral literal) cs
        newConstrs = map (filter (not . eqVariableName name)) unsatConstrs
        newVariables = filter (not . eqVariableName name) ls
        newLiterals = literal:as

unsatLiteral :: Eq a => Literal a -> Constr a -> Bool
unsatLiteral lit constrs = not (or literals)
    where 
        (Literal name _) = lit
        ls = filter (eqVariableName name) constrs
        literals = map (variableSat lit) ls
