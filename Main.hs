import Solver
import Test.HUnit

x1 = newVar "x1" False
x2 = newVar "x2" False
x3 = newVar "x3" False
x4 = newVar "x4" False
x5 = newVar "x5" False
x6 = newVar "x6" False
x7 = newVar "x7" False
x8 = newVar "x8" False

c1 = [x1, notLiteral x2]
c2 = [x2, x3]
c3 = [x4, x5, notLiteral x8]
c4 = [x5, notLiteral x6, x7]
c5 = [x2, notLiteral x6, notLiteral x7]
c6 = [x8]

model1 = Model [c1, c2, c3, c4, c5, c6] [x1, x2, x3, x4, x5, x6, x7, x8]
model2 = Model [[notLiteral x1], [x2]] [x1, x2]
model3 = Model [[notLiteral x1], [x1]] [x1]

main :: IO ()
main = do
    let sol = solve model1
    case sol of 
        Just ls -> do
            putStrLn "Solution"
            printList ls
        Nothing -> 
            putStrLn "No Solution"

printList :: Show a => [a] -> IO ()
printList [] = putStrLn ""
printList (l:ls) = do
    print l
    printList ls
