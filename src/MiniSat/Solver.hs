module MiniSat.Solver (Model(..), Literal(..), Variable(..), Sign(..), solve, notVariable, newModel) where
import Data.List (nub)

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

variableTrue :: a -> Bool -> Variable a
variableTrue name True = Variable name None
variableTrue name False = Variable name Not

data Constr a = Constr [Variable a] [Literal a]
    deriving Show

setLiteral :: Eq a => Literal a -> Constr a -> Constr a
setLiteral literal (Constr vars lits) = Constr newVars (literal:lits)
    where 
        Literal name _ = literal
        newVars = filter (not . eqVariableName name) vars

unsatLiteral :: Eq a => Literal a -> Constr a -> Bool
unsatLiteral lit (Constr vars _) = not (or literals)
    where 
        (Literal name _) = lit
        nameVars = filter (eqVariableName name) vars
        literals = map (variableSat lit) nameVars

inConstr :: Eq a => a -> Constr a -> Bool
inConstr name (Constr vars _) = name `elem` names
    where 
        names = map (\(Variable n _) -> n) vars


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

data AssignNode a = Node (Literal a) [AssignNode a]
    deriving Show

newtype AssignTree a = Tree [AssignNode a]
    deriving Show

addLiteral :: Eq a => AssignTree a -> Literal a -> [Literal a] -> AssignTree a
addLiteral (Tree ts) lit impl = Tree (newNode:ts)
    where 
        implNodes = filter (\(Node t _) -> t `elem` impl) ts
        newNode = Node lit implNodes

literalConflict :: Eq a => AssignTree a -> Literal a -> [Literal a]
literalConflict (Tree ts) (Literal name _) 
    | null literalNodes = []
    | otherwise = nub literals
    where 
        literalNodes = filter (\(Node (Literal n _) _) -> name == n) ts
        literals = map (\(Node l _) -> l) literalNodes


data Model a = Model [Constr a] [Variable a] [Literal a] (AssignTree a)
    deriving Show

newModel :: [[Variable a]] -> [Variable a] -> Model a
newModel clauses vars = model
    where 
        model = Model constrs vars [] (Tree [])
        constrs = map (\v -> Constr v []) clauses

data Status a = Valid (Model a) | Conflict (Constr a)

solve :: Eq a => Model a -> Maybe [Literal a]
solve model = case solveModel model of 
    Valid result -> Just assigns
        where
            Model _ _ assigns _ = result

    Conflict _ -> Nothing

solveModel :: Eq a => Model a -> Status a
solveModel model@(Model _ [] _ _) = Valid model
solveModel model@(Model _ (var:_) _ _) = case modelTrue of
    Valid _ -> modelTrue
    Conflict conflict -> conflictModel model var conflict
    where
        (Variable name _) = var

        lTrue = Literal name True
        modelTrue = assignVariable model lTrue

conflictModel :: Eq a => Model a -> Variable a -> Constr a -> Status a
conflictModel (Model cs ls as tree) (Variable name _) conflict = if inConstr name conflict then modelFalse else Conflict conflict
    where 
        lFalse = Literal name False
        updateModel = Model (conflict:cs) ls as tree
        modelFalse = assignVariable updateModel lFalse

assignVariable :: Eq a => Model a -> Literal a -> Status a
assignVariable model assign = maybe result Conflict maybeConflict
    where
        updateModel = updateVariable model assign []
        propModel = propagateUnitConstr updateModel
        maybeConflict = constrConflict propModel

        result = solveModel propModel

constrConflict :: Eq a => Model a -> Maybe (Constr a)
constrConflict (Model constrs _ _ tree) 
    | null vars = Nothing
    | otherwise = Just conflict
    where 
        vars = filter (\(Constr var _) -> null var) constrs
        Constr _ lits = head vars
        declLits = concatMap (literalConflict tree) lits
        declVars = map (\(Literal name val) -> variableTrue name val) declLits 
        conflict = Constr declVars []

propagateUnitConstr :: Eq a => Model a -> Model a
propagateUnitConstr model 
    | null unit = model 
    | otherwise = propagateUnitConstr updateModel
    where
        (Model cs _ _ _) = model
        unit = filter unitConstr cs

        Constr constr impl = head unit
        (Variable name sign) = head constr
        assign = literalTrue name sign

        updateModel = updateVariable model assign impl

unitConstr :: Constr a -> Bool
unitConstr (Constr vars _) = length vars == 1

updateVariable :: Eq a => Model a -> Literal a -> [Literal a] -> Model a
updateVariable (Model vars literals assigns tree) literal impl = updateModel
    where
        Literal name _ = literal
        unsatConstrs = filter (unsatLiteral literal) vars
        newVars = map (setLiteral literal) unsatConstrs        
        newLiterals = filter (not . eqVariableName name) literals
        newAssigns = literal:assigns
        newTree = addLiteral tree literal impl

        updateModel = Model newVars newLiterals newAssigns newTree

