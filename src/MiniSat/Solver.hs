module MiniSat.Solver (Model(..), Literal(..), Variable(..), Sign(..), solve, notVariable, newModel) where
import Data.List (nub)

data Sign = None | Not
    deriving (Show, Eq) 

flipSign :: Sign -> Sign
flipSign None = Not
flipSign Not = None

data Variable a = Variable a Sign
    deriving (Show, Eq)

notVariable :: Variable a -> Variable a
notVariable (Variable name sign) = Variable name (flipSign sign)

eqVariableName :: (Show a, Eq a) => a -> Variable a -> Bool 
eqVariableName n1 (Variable n2 _) = n1 == n2

data Constr a = Constr [Variable a] [Literal a]
    deriving (Show, Eq)

setLiteral :: (Show a, Eq a) => Literal a -> Constr a -> Constr a
setLiteral literal (Constr vars lits) = Constr newVars newLiterals
    where 
        Literal name _ = literal
        newVars = filter (not . eqVariableName name) vars
        exists = filter (eqVariableName name) vars
        newLiterals = if null exists then lits else literal:lits

satLiteral :: (Show a, Eq a) => Literal a -> Constr a -> Bool
satLiteral lit (Constr vars _) = or literals
    where 
        (Literal name _) = lit
        nameVars = filter (eqVariableName name) vars
        literals = map (variableSat lit) nameVars

data Literal a = Literal a Bool
    deriving (Eq, Show, Ord)

literalTrue :: a -> Sign -> Literal a
literalTrue name None = Literal name True
literalTrue name Not = Literal name False 

variableSat :: Literal a -> Variable a -> Bool
variableSat (Literal _ val) (Variable _ None) = val
variableSat (Literal _ val) (Variable _ Not) = not val

literalNegation :: Literal a -> Variable a
literalNegation (Literal name False) = Variable name None
literalNegation (Literal name True) = Variable name Not

data AssignNode a = Node (Literal a) [Literal a]
    deriving Show

newtype AssignTree a = Tree [AssignNode a]
    deriving Show

addLiteral :: (Show a, Eq a) => AssignTree a -> Literal a -> [Literal a] -> AssignTree a
addLiteral (Tree ts) lit impl = Tree (newNode:ts)
    where 
        implNodes = filter (\(Node t _) -> t `elem` impl) ts
        implLits = nub (concatMap (\(Node _ l) -> l) implNodes)
        newNode = if null implLits then Node lit [lit] else Node lit implLits 

literalConflict :: (Show a, Eq a) => AssignTree a -> Literal a -> [Literal a]
literalConflict (Tree ts) (Literal name _) 
    | null literalNodes = []
    | otherwise = literals
    where 
        literalNodes = filter (\(Node (Literal n _) _) -> name == n) ts
        (Node _ literals) = head literalNodes

data Model a = Model [Constr a] [Variable a] [Literal a] (AssignTree a)
    deriving Show

newModel :: (Show a, Eq a) => [[Variable a]] -> [Variable a] -> Model a
newModel clauses vars = model
    where 
        model = Model constrs vars [] (Tree [])
        constrs = map (\v -> Constr v []) clauses

maxConflicts :: Int
maxConflicts = 10

conflictsToClauses :: (Show a, Eq a) => Model a -> [Constr a] -> Model a 
conflictsToClauses (Model constrs vars literals tree) learnt = updateModel
    where 
        updateModel = Model (clauses ++ constrs) vars literals tree
        clauses = map (\c -> foldr setLiteral c literals) learnt

data Status a = Valid (Model a) | Conflict (Constr a) [Constr a]
data Assign a = Update (Model a) | Clause (Constr a)

solve :: (Show a, Eq a) => Model a -> Maybe [Literal a]
solve model = case solveModel model of
    Valid (Model _ _ ls _) -> Just ls 
    Conflict _ _ -> Nothing

solveModel :: (Show a, Eq a) => Model a -> Status a
solveModel model = case propagateUnits model of
    Update propModel -> decideLiteral propModel 
    Clause conflict -> Conflict conflict [conflict]

decideLiteral :: (Show a, Eq a) => Model a -> Status a
decideLiteral model 
    | null vars = Valid model
    | otherwise = case solveModel modelTrue of 
        Valid updateModel -> Valid updateModel
        Conflict conflict learnt -> analyzeConflict model conflict learnt
    where 
        (Model _ vars _ _) = model
        (Variable name _) = head vars

        lTrue = Literal name True
        modelTrue = assignLiteral model lTrue []

analyzeConflict :: (Show a, Eq a) => Model a -> Constr a -> [Constr a] -> Status a
analyzeConflict model conflict learnt 
    | conflictExists = Conflict conflict learnt
    | otherwise = case solveModel updateModel of
        Valid model2 -> Valid model2
        Conflict conflict2 newLearnt -> Conflict conflict2 (newLearnt ++ learnt)
    where 
        (Model _ _ literals _) = model
        (Constr vars _) = foldr setLiteral conflict literals
        conflictExists = null vars
        updateModel = conflictsToClauses model learnt

conflictConstr :: (Show a, Eq a) => Model a -> Maybe (Constr a)
conflictConstr (Model constrs _ _ tree)
    | null conflictVars = Nothing
    | otherwise = Just conflict 
    where 
        unitConstrs = filter unitConstr constrs
        unitVars = nub (map (\(Constr vs _) -> head vs) unitConstrs)
        conflictVars = filter (\v -> countOccurences compName v unitVars == 2) unitVars

        (Variable name _) = head conflictVars
        conflictConstrs = filter (constrUnitVariable name) unitConstrs
        conflictLiterals = concatMap (\(Constr _ lit) -> lit) conflictConstrs
        literals = nub (concatMap (literalConflict tree) conflictLiterals)
        conflict = Constr (map literalNegation literals) []

countOccurences :: (a -> b -> Bool) -> a -> [b] -> Int 
countOccurences f s = foldr (\item n -> if f s item then n + 1 else n) 0 

constrUnitVariable :: Eq a => a -> Constr a -> Bool
constrUnitVariable n1 (Constr vars _) 
    | null vars = False
    | otherwise = n1 == n2
    where 
        Variable n2 _ = head vars

compName :: (Show a, Eq a) => Variable a -> Variable a -> Bool
compName (Variable n1 _) (Variable n2 _) = n1 == n2

propagateUnits :: (Show a, Eq a) => Model a -> Assign a
propagateUnits model 
    | null unit = Update model
    | otherwise = case maybeConflict of 
        Just conflict -> Clause conflict
        Nothing -> propagateUnits updateModel
    where
        Model constrs _ _ _ = model
        unit = filter unitConstr constrs

        Constr constr impl = head unit
        (Variable name sign) = head constr
        assign = literalTrue name sign

        maybeConflict = conflictConstr model
        updateModel = assignLiteral model assign impl

unitConstr :: Constr a -> Bool
unitConstr (Constr vars _) = length vars == 1

assignLiteral :: (Show a, Eq a) => Model a -> Literal a -> [Literal a] -> Model a
assignLiteral (Model constrs vars literals tree) var impl = updateModel
    where
        Literal name _ = var
        unsatConstrs = filter (not . satLiteral var) constrs
        newConstrs = map (setLiteral var) unsatConstrs        

        newVars = filter (not . eqVariableName name) vars
        newLiterals = var:literals
        newTree =  addLiteral tree var impl

        updateModel = Model newConstrs newVars newLiterals newTree

