module MiniSat.Solver (Model(..), Literal(..), Variable(..), Sign(..), solve, setLiteral, notVariable, newModel) where
import Data.List (nub)

data Sign = None | Not
    deriving (Show, Eq) 

flipSign :: Sign -> Sign
flipSign None = Not
flipSign Not = None

-- | An unbound variable in a clause, with sign Sign. 
--   Variables are identified using the label of type a, 
--   which should be an instance of Eq. 
data Variable a = Variable a Sign
    deriving (Show, Eq)

notVariable :: Variable a -> Variable a
notVariable (Variable name sign) = Variable name (flipSign sign)

eqVariableName :: Eq a => a -> Variable a -> Bool 
eqVariableName n1 (Variable n2 _) = n1 == n2

-- | A literal is the assignment of an identifier to True or False
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

-- | A clause of an expression in CNF. [Variable a]
--   is a list of unbound variables in the clause, 
--   and [Literal a] is a list of the assigned variables. 
--   
--   When a variable is decided to be True or False, if 
--   the clause is satisfied, the clause is removed from 
--   the expression, otherwise the variable is removed 
--   from the variable list and the assignment is added
--   to the literal list.
--
--      Constr [Variable "x1" None, Variable "x2" Not, Variable "x3" Not] [] 
--
--   represents the clause 
--    
--      x1 or (not x2) or (not x3)
--
data Constr a = Constr [Variable a] [Literal a]
    deriving (Show, Eq)

updateConstrLiteral :: Eq a => Literal a -> Constr a -> Constr a
updateConstrLiteral literal (Constr vars literals) = Constr newVars newLiterals
    where 
        Literal name _ = literal
        newVars = filter (not . eqVariableName name) vars
        exists = filter (eqVariableName name) vars
        newLiterals = if null exists then literals else literal:literals

satLiteral :: Eq a => Literal a -> Constr a -> Bool
satLiteral lit (Constr vars _) = or literals
    where 
        (Literal name _) = lit
        nameVars = filter (eqVariableName name) vars
        literals = map (variableSat lit) nameVars

-- | AssignNode stores the literals which caused a literal 
--   to become unit in the expression. 
--
--   If we have the clause 
--
--      x1 or (not x2) or x3
--   
--   and we decide x2 is True, and x3 is False.
--   To satisfy the clause we must have x1 is True, so we 
--   get the assign node 
--
--      Node (Literal "x1" True) [Literal "x2" True, Literal "x3" False]
--   
--   This is used to construct learnt clauses.
--
data AssignNode a = Node (Literal a) [Literal a]
    deriving Show

-- | AssignTree stores an assign node for each assigned literal.
newtype AssignTree a = Tree [AssignNode a]
    deriving Show

addLiteral :: Eq a => AssignTree a -> Literal a -> [Literal a] -> AssignTree a
addLiteral (Tree ts) lit impl = Tree (newNode:ts)
    where 
        implNodes = filter (\(Node t _) -> t `elem` impl) ts
        implLits = nub (concatMap (\(Node _ l) -> l) implNodes)
        newNode = if null implLits then Node lit [lit] else Node lit implLits 

-- | literalConflict constructs a conflict clauses using the assign tree for 
--   a given literal. Returns a list of literals which caused the assignment. 
literalConflict :: Eq a => AssignTree a -> Literal a -> [Literal a]
literalConflict (Tree ts) (Literal name _) 
    | null literalNodes = []
    | otherwise = literals
    where 
        literalNodes = filter (\(Node (Literal n _) _) -> name == n) ts
        (Node _ literals) = head literalNodes

-- | Model stores the state of the solver. 
--
--      Model constrs vars literals tree 
--   
--   has a list of clauses 'constrs' which store 
data Model a = Model [Constr a] [Variable a] [Literal a] (AssignTree a)
    deriving Show

newModel :: Eq a => [[Variable a]] -> [Variable a] -> Model a
newModel clauses vars = model
    where 
        model = Model constrs vars [] (Tree [])
        constrs = map (\v -> Constr v []) clauses

maxConflicts :: Int
maxConflicts = 10

-- | Add learnt clauses to the model
conflictsToClauses :: Eq a => Model a -> [Constr a] -> Model a 
conflictsToClauses (Model constrs vars literals tree) learnt = updateModel
    where 
        updateModel = Model (clauses ++ constrs) vars literals tree
        clauses = map (\c -> foldr updateConstrLiteral c literals) learnt

-- | Status is the result of trying to find a solution to the model. 
--   Either the model is valid or a conflict occurs. 
data Status a = Valid (Model a) | Conflict (Constr a) [Constr a]

-- | Result of propagating unit constraints, either the model is 
--   valid or a conflict occurs.
data Assign a = Update (Model a) | Clause (Constr a)

solve :: Eq a => Model a -> Maybe [Literal a]
solve model = case simplifyModel model of 
    Valid updateModel -> case solveModel updateModel of
        Valid (Model _ _ ls _) -> Just ls 
        Conflict _ _ -> Nothing
    Conflict _ _ -> Nothing

setLiteral :: Eq a => Model a -> Literal a -> Maybe (Model a)
setLiteral model literal = case propagateUnits assignModel of 
    Update propModel -> Just propModel
    Clause _ -> Nothing
    where 
        assignModel = assignLiteral model literal []

simplifyModel :: Eq a => Model a -> Status a
simplifyModel model = Valid model

-- | solveModel is an iteration of the solver. Firstly
--   propagate any unit constraints, 
--
--    If a conflict is found, return the conflict
--
--    If no conflicts, decide a new literal
--
solveModel :: Eq a => Model a -> Status a
solveModel model = case propagateUnits model of
    Update propModel -> decideLiteral propModel 
    Clause conflict -> Conflict conflict [conflict]

-- | Assign the first unbound variable in the model 
--   to true and attempt to solve the resulting model. 
--
--    If a conflict occurs, analyze the conflict to either 
--    continue the search or return the conflict 
--
--    If no conflict occurs, return the valid model.
--
decideLiteral :: Eq a => Model a -> Status a
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

-- | analyzeConflict checks if the constraint 'conflict' has an unbound variable 
--   under 'model'. If not, the conflict is returned, otherwise the conflict and 
--   learnt are added to the model and search resumes.
analyzeConflict :: Eq a => Model a -> Constr a -> [Constr a] -> Status a
analyzeConflict model conflict learnt 
    | conflictExists = Conflict conflict learnt
    | otherwise = case solveModel updateModel of
        Valid model2 -> Valid model2
        Conflict conflict2 newLearnt -> Conflict conflict2 (newLearnt ++ learnt)
    where 
        (Model _ _ literals _) = model
        (Constr vars _) = foldr updateConstrLiteral conflict literals
        conflictExists = null vars
        updateModel = conflictsToClauses model learnt

-- | conflictConstr returns Nothing if the model has no conflicts, and 
--   Just constr if there is a conflict, where constr is the learnt clause.
--
--   A conflict exists if x and not x appear in unit clauses.
conflictConstr :: Eq a => Model a -> Maybe (Constr a)
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

compName :: Eq a => Variable a -> Variable a -> Bool
compName (Variable n1 _) (Variable n2 _) = n1 == n2

propagateUnits :: Eq a => Model a -> Assign a
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

assignLiteral :: Eq a => Model a -> Literal a -> [Literal a] -> Model a
assignLiteral (Model constrs vars literals tree) var impl = updateModel
    where
        Literal name _ = var
        unsatConstrs = filter (not . satLiteral var) constrs
        newConstrs = map (updateConstrLiteral var) unsatConstrs        

        newVars = filter (not . eqVariableName name) vars
        newLiterals = var:literals
        newTree =  addLiteral tree var impl

        updateModel = Model newConstrs newVars newLiterals newTree

