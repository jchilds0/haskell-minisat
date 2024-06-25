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

eqVariableName :: Eq a => a -> Variable a -> Bool 
eqVariableName n1 (Variable n2 _) = n1 == n2

data Constr a = Constr [Variable a] [Literal a]
    deriving Show

setLiteral :: Eq a => Literal a -> Constr a -> Constr a
setLiteral literal (Constr vars lits) = Constr newVars newLiterals
    where 
        Literal name _ = literal
        newVars = filter (not . eqVariableName name) vars
        exists = filter (eqVariableName name) vars
        newLiterals = if null exists then lits else literal:lits

satLiteral :: Eq a => Literal a -> Constr a -> Bool
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

data Model a = Model [Constr a] [Variable a] [Constr a] [Literal a] (AssignTree a)
    deriving Show

newModel :: Eq a => [[Variable a]] -> [Variable a] -> Model a
newModel clauses vars = model
    where 
        model = Model constrs vars [] [] (Tree [])
        constrs = map (\v -> Constr v []) clauses

maxConflicts :: Int
maxConflicts = 100

addConflictClauses :: Eq a => Model a -> Model a
addConflictClauses (Model constrs vars conflicts literals tree) = updateModel
    where
        updateModel = Model (newConstrs ++ constrs) vars newConflicts literals tree

        newConflicts = conflicts
        newConstrs = map (\conflict -> foldr setLiteral conflict literals) newConflicts

data Status a = Valid (Model a) | Conflict (Constr a)

solve :: Eq a => Model a -> Maybe [Literal a]
solve model = case solveModel propResult of
        Valid (Model _ _ _ ls _) -> Just ls 
        Conflict _ -> Nothing
    where 
        propResult = propagateUnits model

assignLiteral :: Eq a => Model a -> Literal a -> Status a
assignLiteral model literal = solveModel (setVariable model literal [])

solveModel :: Eq a => Status a -> Status a
solveModel (Conflict conflict) = Conflict conflict
solveModel (Valid model) 
    | null vars = Valid model
    | otherwise = case assignLiteral model lTrue of
        Valid modelTrue -> Valid modelTrue
        Conflict conflict -> handleConflict model var conflict
    where
        (Model _ vars _ _ _) = model
        var = head vars
        (Variable name _) = var
        lTrue = Literal name True

handleConflict :: Eq a => Model a -> Variable a -> Constr a -> Status a
handleConflict model (Variable name _) conflict 
    | conflictExists = Conflict conflict
    | otherwise = assignLiteral updateModel lFalse
    where 
        (Model _ _ _ literals _) = model

        (Constr vars _) = foldr setLiteral conflict literals
        conflictExists = not (null vars)
        
        lFalse = Literal name False
        updateModel = addConflictClauses model 

countOccurences :: (a -> b -> Bool) -> a -> [b] -> Int 
countOccurences f s = foldr (\item n -> if f s item then n + 1 else n) 0 

compName :: Eq a => Variable a -> Variable a -> Bool
compName (Variable n1 _) (Variable n2 _) = n1 == n2

constrConflict :: Eq a => Model a -> Maybe (Constr a)
constrConflict (Model constrs _ _ _ tree) 
    | length unitVars <= 1 = Nothing
    | null conflictVars = Nothing
    | otherwise = Just conflict
    where 
        unitConstrs = filter unitConstr constrs
        unitVars = nub (concatMap (\(Constr vars _) -> vars) unitConstrs)
        varCounts = map (\var -> (countOccurences compName var unitVars, var)) unitVars
        conflictVars = filter ((==2) . fst) varCounts

        (Variable name _) = snd (head conflictVars)
        declLits = concatMap (literalConflict tree) [Literal name True, Literal name False]
        conflict = Constr (map literalNegation declLits) []

propagateUnits :: Eq a => Model a -> Status a
propagateUnits model 
    | null unit = Valid model 
    | otherwise = maybe propModel Conflict maybeConflict
    where
        (Model constrs _ _ _ _) = model
        unit = filter unitConstr constrs

        Constr constr impl = head unit
        (Variable name sign) = head constr
        assign = literalTrue name sign

        maybeConflict = constrConflict model
        propModel = setVariable model assign impl

unitConstr :: Constr a -> Bool
unitConstr (Constr vars _) = length vars == 1

setVariable :: Eq a => Model a -> Literal a -> [Literal a] -> Status a
setVariable (Model constrs vars conflicts literals tree) var impl = propagateUnits updateModel
    where
        Literal name _ = var
        unsatConstrs = filter (not . satLiteral var) constrs
        newConstrs = map (setLiteral var) unsatConstrs        
        newVars = filter (not . eqVariableName name) vars
        newLiterals = var:literals
        newTree = addLiteral tree var impl

        updateModel = Model newConstrs newVars conflicts newLiterals newTree

