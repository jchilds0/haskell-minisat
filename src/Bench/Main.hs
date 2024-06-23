module Main where

-- Benchmark CNFs from https://www.cs.ubc.ca/~hoos/SATLIB/benchm.html

import Data.Time.Clock
import MiniSat.Solver
import MiniSat.Parser

dir = "./src/Bench/unfsat/"

main :: IO ()
main = do
    -- testFile "dubois22.cnf"

    putStrLn "--- 20 Variables ---"
    let file1 = map (\x -> "uf20-0" ++ show x ++ ".cnf") [1..10]
    mapM_ testFile file1

    putStrLn "--- 50 Variables ---"
    let file2 = map (\x -> "uf50-0" ++ show x ++ ".cnf") [1..10]
    mapM_ testFile file2

    putStrLn "--- 75 Variables ---"
    let file3 = map (\x -> "uf75-0" ++ show x ++ ".cnf") [1..10]
    mapM_ testFile file3

    putStrLn "--- 100 Variables ---"
    let file3 = map (\x -> "uf100-0" ++ show x ++ ".cnf") [1..10]
    mapM_ testFile file3

    -- putStrLn "--- 150 Variables ---"
    -- let file3 = map (\x -> "uf150-0" ++ show x ++ ".cnf") [1..10]
    -- mapM_ testFile file3


testFile :: String -> IO ()
testFile fileName = do
    rows <- readFile (dir ++ fileName)
    let ls = lines rows
    let model = dimacsToModel ls

    putStr $ "\t" ++ fileName ++ ": "

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
