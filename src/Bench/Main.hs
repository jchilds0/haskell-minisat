module Main where

-- Benchmark CNFs from https://www.cs.ubc.ca/~hoos/SATLIB/benchm.html
-- Extract the benchmarks tar first using
-- 
--    tar -xvf benchmarks.tar.gz

import Data.Time.Clock
import MiniSat.Solver
import Numeric
import MiniSat.Parser
import System.Directory
import Control.Concurrent.PooledIO.Independent
import GHC.GHCi.Helpers (flushAll)

dir :: String
dir = "./benchmarks/"

numTests :: Int
numTests = 10

testSets :: [(String, Bool)]
testSets = [
        -- Uniform Random
        ("uf20-91/", True), 
        ("uf50-218/", True), 
        ("uf75-325/", True),
        ("uf100-430/", True), 
        ("uf125-538/", True),
        ("uf150-645/", True), 
        ("uf175-753/", True),
        ("uf200-860/", True), 
        ("uf225-960/", True),
        ("uf250-1065/", True),
        
        ("uuf50-218/", False),
        ("uuf75-325/", False),
        ("uuf100-430/", False),
        ("uuf125-538/", False),
        ("uuf150-645/", False),
        ("uuf175-753/", False),
        ("uuf200-860/", False),
        ("uuf225-960/", False),
        ("uuf250-1065/", False)

        -- Random + Backbone minimal
        -- ("RTI_k3_n100_m429/", True),
        -- ("BMS_k3_n100_m429/", True),

        -- Random + Controlled backbone
        -- ("CBS_k3_n100_m403_b10/", True),
        -- ("CBS_k3_n100_m403_b30/", True),
        -- ("CBS_k3_n100_m403_b50/", True)
    ]

main :: IO ()
main = do
    -- testFile "dubois22.cnf"
    let files = map fst testSets
    let times = map (\(f, sat) -> testFiles (dir ++ f) sat) testSets
    let results = zipWith printResult times files
    sequence_ results

testFiles :: String -> Bool -> IO Double
testFiles path sat = do
    names <- getDirectoryContents path
    let fileNames = filter (\n -> n `notElem` [".", ".."]) names
    let files = map (path ++) (take numTests fileNames)
    times <- mapM (testFile sat) files

    return (avg times)

avg :: [Double] -> Double
avg ls = sum ls / fromIntegral (length ls)

testFile :: Bool -> String -> IO Double
testFile sat file = do
    rows <- readFile file
    let ls = lines rows
    let model = dimacsToModel ls

    start <- getCurrentTime
    let sol = solve model
    case sol of 
        Just _ -> if not sat then putStrLn $ file ++ " is not sat" else putStr ""
        Nothing -> if sat then putStrLn $ file ++ " is sat" else putStr ""

    end <- getCurrentTime
    let timeDiff = diffUTCTime end start
    return (realToFrac timeDiff)


printResult :: IO Double -> String -> IO ()
printResult t file = do 
    putStr $ file ++ "\t" 
    time <- t
    printTime time
    putStrLn ""

printTime :: Double -> IO ()
printTime timeS = do 
    let timeMS = 1000 * timeS
    if timeS < 1 then
        putStr $ showFFloat (Just 2) timeMS "" ++ " ms"
    else 
        putStr $ showFFloat (Just 2) timeS "" ++ " s"


printList :: Show a => [a] -> IO ()
printList [] = putStrLn ""
printList (l:ls) = do
    print l
    printList ls
