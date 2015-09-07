module Prime where

import Control.Monad (forever)
import System.IO


primes :: IO ()
primes = do
    let pFile = "Primes.txt"
    pContent <- readFile pFile
    let pList = read pContent :: [Integer]
    let pTest = (last pList) + 2
    primeLoop pTest pList pFile pTest
        

----------------------------------------------------

isPrime :: Integer -> [Integer] -> Bool
isPrime _ [] = True
isPrime start (i:is)
    | (fromIntegral start) / (fromIntegral i) >= 2.0 =
        if (start `mod` i) /= 0
            then isPrime start is
            else False
    | otherwise = True

addPrime :: Integer -> [Integer] -> [Integer]
addPrime int is = is ++ [int]

primeLoop :: Integer -> [Integer] -> FilePath -> Integer -> IO ()
primeLoop latest soFar pFile buffer = do
    let latest' = if latest `mod` 5 == 0 then latest + 2 else latest
    if isPrime latest' soFar
        then do
            let soFar' = addPrime latest' soFar
            let latest'' = latest' + 2
            putStrLn $ "\nWE GOT ONE! (" ++ show latest' ++ ")\n"
            if latest > (buffer + 50000)
                then do
                    writeFile pFile $ show $ soFar'
                    primeLoop latest'' soFar' pFile latest''
                else primeLoop latest'' soFar' pFile buffer
        else do
            let latest'' = latest' + 2
            putStrLn "NOPE"
            primeLoop latest'' soFar pFile buffer