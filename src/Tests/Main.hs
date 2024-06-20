module Main (main) where

import MiniSat.Solver
import Test.HUnit
import qualified System.Exit as Exit

main :: IO ()
main = do 
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess

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
c5 = [notLiteral x2, notLiteral x6, notLiteral x7]
c6 = [x8]

ans1 = Assign "x1" True
ans2 = Assign "x2" True
ans3 = Assign "x3" True
ans4 = Assign "x4" True
ans5 = Assign "x5" True
ans6 = Assign "x6" True
ans7 = Assign "x7" True
ans8 = Assign "x8" True

model1 = Model [c1, c2, c3, c4, c5, c6] [x1, x2, x3, x4, x5, x6, x7, x8] []
sol1 = [ans1, ans2, ans3, ans4, ans5, ans6, ans7, ans8]
test1 = TestCase (assertEqual "large sat" (Just sol1) (solve model1))

model2 = Model [[notLiteral x1], [x2]] [x1, x2] []
sol2 = [Assign "x1" False, Assign "x2" True]
test2 = TestCase (assertEqual "small sat" (Just sol2) (solve model2))

model3 = Model [[notLiteral x1], [x1]] [x1] []
test3 = TestCase (assertEqual "no sat" (solve model3) Nothing)

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3]
