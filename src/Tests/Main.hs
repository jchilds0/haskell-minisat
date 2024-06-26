module Main (main) where

import MiniSat.Solver
import LargeUnSat 
import LargeSat
import Test.HUnit
import qualified System.Exit as Exit
import Data.List (sort)

main :: IO ()
main = do 
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess

testModel :: (Show a, Eq a) => Model a -> Bool
testModel model = case solve model of
    Just _ -> True
    Nothing -> False

x1 = Variable "x1" None 
x2 = Variable "x2" None 
x3 = Variable "x3" None 
x4 = Variable "x4" None 
x5 = Variable "x5" None 
x6 = Variable "x6" None 
x7 = Variable "x7" None 
x8 = Variable "x8" None 
x9 = Variable "x9" None 
x10 = Variable "x10" None 
x11 = Variable "x11" None 
x12 = Variable "x12" None 
x13 = Variable "x13" None 

c1 = [x1, notVariable x2]
c2 = [x2, x3]
c3 = [x4, x5, notVariable x8]
c4 = [x5, notVariable x6, x7]
c5 = [x2, notVariable x6, notVariable x7]
c6 = [x8]

ans1 = Literal "x1" True
ans2 = Literal "x2" True
ans3 = Literal "x3" True
ans4 = Literal "x4" True
ans5 = Literal "x5" True
ans6 = Literal "x6" True
ans7 = Literal "x7" True
ans8 = Literal "x8" True

model1 = newModel [c1, c2, c3, c4, c5, c6] [x1, x2, x3, x4, x5, x6, x7, x8]
sol1 = [ans1, ans2, ans3, ans4, ans5, ans6, ans7, ans8]
test1 = TestCase (assertEqual "small sat 1" (Just sol1) (sort <$> solve model1))

model2 = newModel [[notVariable x1], [x2]] [x1, x2]
sol2 = [Literal "x1" False, Literal "x2" True]
test2 = TestCase (assertEqual "small sat 2" (Just sol2) (sort <$> solve model2))

model3 = newModel [[notVariable x1], [x1]] [x1]
test3 = TestCase (assertEqual "no sat" Nothing (solve model3))

test4 = TestCase (assertEqual "large sat" True (testModel largeSat))
test5 = TestCase (assertEqual "large unsat" False (testModel largeUnSat))

tests = TestList [
        TestLabel "test1" test1, 
        TestLabel "test2" test2, 
        TestLabel "test3" test3,
        TestLabel "test4" test4,
        TestLabel "test5" test5
    ]
