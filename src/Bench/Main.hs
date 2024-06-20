module Main where

-- Benchmark CNFs from https://www.cs.ubc.ca/~hoos/SATLIB/benchm.html

import Data.Time.Clock
import MiniSat.Solver
import MiniSat.Parser

dir = "./src/Bench/unfsat/"

main :: IO ()
main = do
    -- testFile "uf20-01.cnf"

    let file1 = map (\x -> "uf20-0" ++ show x ++ ".cnf") [1..10]
    mapM_ testFile file1

    -- let file2 = map (\x -> "uf50-0" ++ show x ++ ".cnf") [1..10]
    -- mapM_ testFile file2

    -- let file3 = map (\x -> dir ++ "uf75-0" ++ show x ++ ".cnf") [1..10]
    -- mapM_ testFile file3

    -- testFile $ dir ++ "dubois22.cnf"

testFile :: String -> IO ()
testFile fileName = do
    rows <- readFile (dir ++ fileName)
    let ls = lines rows
    let model = dimacsToModel ls

    putStr $ fileName ++ ": "

    start <- getCurrentTime
    let sol = solve model
    case sol of 
        Just ls -> do
            putStr "Solution   "
        Nothing -> 
            putStr "No Solution"

    end <- getCurrentTime
    putStrLn $ "\t(Took: " ++ show (diffUTCTime end start) ++ ")"

printList :: Show a => [a] -> IO ()
printList [] = putStrLn ""
printList (l:ls) = do
    print l
    printList ls
