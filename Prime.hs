module Prime where

import Control.Monad (forever)
import System.IO


primes :: IO ()
primes = forever $ do
    let pFile = "Primes.txt"
    let iFile = "It.txt"
    pContent <- readFile pFile
    pIt <- readFile iFile
    let pList = read pContent :: [Integer]
    let pTest' = read pIt :: Integer
    let pTest = if pTest' `mod` 5 == 0 then pTest' + 2 else pTest'
    if isPrime pTest pList
        then do
            writeFile pFile $ show $ addPrime pTest pList
            writeFile iFile $ show $ pTest + 2
            putStrLn $ "\nWE GOT ONE! (" ++ show pTest ++ ")\n"
        else do
            writeFile iFile $ show $ pTest + 2
            putStrLn "NOPE"

----------------------------------------------------

isPrime :: Integer -> [Integer] -> Bool
isPrime _ [] = True
isPrime start (i:is)
    | (start `mod` i) /= 0 = isPrime start is
    | otherwise = False

addPrime :: Integer -> [Integer] -> [Integer]
addPrime int is = is ++ [int]