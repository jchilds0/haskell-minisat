module MiniSat.Solver (Model(..), Variable(..), Sign(..), solve, notVariable, newModel) where

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

eqLiteralAssignNode :: Eq a => Literal a -> AssignNode a -> Bool
eqLiteralAssignNode (Literal n1 _) (Node (Literal n2 _) _) = n1 == n2

addLiteral :: Eq a => AssignTree a -> Literal a -> AssignTree a
addLiteral (Tree ts) lit 
    | null names = Tree (newNode:ts)
    | otherwise = Tree ts
    where 
        names = filter (eqLiteralAssignNode lit) ts
        newNode = Node lit []

addImplication :: Eq a => AssignTree a -> Literal a -> [Literal a] -> AssignTree a
addImplication (Tree ts) lit impl = Tree (newNode:ts)
    where 
        implNodes = filter (\(Node t _) -> t `elem` impl) ts
        newNode = Node lit implNodes

nodeLeaves :: Eq a => AssignNode a -> [Literal a]
nodeLeaves (Node lit []) = [lit]
nodeLeaves (Node _ cs) = concatMap nodeLeaves cs

literalLeaves :: Eq a => AssignTree a -> a -> [Literal a]
literalLeaves (Tree ts) name
    | null child = []
    | otherwise = nodeLeaves (head child)
    where 
        child = filter (\(Node (Literal n _) _) -> name == n) ts


data Model a = Model [Constr a] [Variable a] [Literal a] (AssignTree a)
    deriving Show

newModel :: [[Variable a]] -> [Variable a] -> Model a
newModel clauses vars = model
    where 
        model = Model constrs vars [] (Tree [])
        constrs = map (\v -> Constr v []) clauses

type SAT = Bool

solve :: Eq a => Model a -> Maybe [Literal a]
solve model = if sat then Just assigns else Nothing
    where
        (result, sat) = solveVar model
        Model _ _ assigns _ = result

solveVar :: Eq a => Model a -> (Model a, SAT)
solveVar model@(Model _ ls _ _) = case ls of
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
  | conflict1 = (updateModel, False)
  | conflict2 = (propModel, False)
  | otherwise = (solveModel, sat)
  where
      declModel = decisionLiteral model assign
      updateModel = updateVariable declModel assign
      conflict1 = constrConflict updateModel

      propModel = propagateUnitConstr updateModel
      conflict2 = constrConflict propModel

      (solveModel, sat) = solveVar propModel

constrConflict :: Model a -> Bool
constrConflict (Model constrs _ _ _) = any null vars
    where 
        vars = map (\(Constr var _) -> var) constrs

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

        implModel = impliedLiteral model assign impl
        updateModel = updateVariable implModel assign

unitConstr :: Constr a -> Bool
unitConstr (Constr vars _) = length vars == 1

decisionLiteral :: Eq a => Model a -> Literal a -> Model a
decisionLiteral (Model cs ls as tree) literal = Model cs ls as newTree
    where 
        newTree = addLiteral tree literal

impliedLiteral :: Eq a => Model a -> Literal a -> [Literal a] -> Model a 
impliedLiteral (Model cs ls as tree) literal impl = Model cs ls as newTree
    where 
        newTree = addImplication tree literal impl

updateVariable :: Eq a => Model a -> Literal a -> Model a
updateVariable (Model cs ls as tree) literal = Model newConstrs newVariables newLiterals tree
    where
        Literal name _ = literal
        unsatConstrs = filter (unsatLiteral literal) cs
        newConstrs = map (setLiteral literal) unsatConstrs        
        newVariables = filter (not . eqVariableName name) ls
        newLiterals = literal:as

