module Prime where

import Control.Monad (forever)
import System.IO

import qualified Data.Sequence as S
import qualified Data.Foldable as F

primes :: IO ()
primes = do
    let pFile = "Primes.txt"
    pContent <- readFile pFile
    let pList' = read pContent :: [Integer]
    let pTest = (last pList') + 2
    let pList = S.fromList pList'
    let sList = makeList pTest pList
    primeLoop pTest pList pFile pTest sList
        
----------------------------------------------------

-- Buffer to space out how often to write to file
bufbuf :: Integer
bufbuf = 300000

-- Make the list to which to check if the Integer is a prime number
makeList :: Integer -> S.Seq Integer -> S.Seq Integer
makeList start is = S.takeWhileL (\x -> x ^ 2 <= start + bufbuf) is

-- Checking if the Integer is a prime number
isPrime :: Integer -> S.Seq Integer -> Bool
isPrime start list = not $ F.any ((==) 0 . mod start) list

-- Add the Integer to the end of the list of primes
addPrime :: Integer -> S.Seq Integer -> S.Seq Integer
addPrime int ints = ints S.|> int

----------------------------------------------------

-- latest = the Integer which should be checked if it's prime
-- soFar = the Sequence of Integers which are prime
-- pFile = the file which to write the soFar-sequence to
-- buffer = the number which doesn't change until latest has moved the amount specified in bufbuf
-- sList = the shortened Sequence of Integers to which to check if latest is a prime number
 
primeLoop :: Integer -> S.Seq Integer -> FilePath -> Integer -> S.Seq Integer -> IO ()
primeLoop latest soFar pFile buffer sList = do
    let latest' = if latest `mod` 5 == 0 then latest + 2 else latest
    if isPrime latest' sList
        then do
            let soFar' = addPrime latest' soFar
            let latest'' = latest' + 2
            putStrLn $ "\nWE GOT ONE! (" ++ show latest' ++ ")\n"
            if latest' > (buffer + bufbuf)
                then do
                    putStrLn "-- Writing to file : Writing to file : Writing to file --"
                    writeFile pFile $ show $ F.toList soFar'
                    let shortList = makeList latest' soFar'
                    primeLoop latest'' soFar' pFile latest'' shortList
                else primeLoop latest'' soFar' pFile buffer sList
        else do
            let latest'' = latest' + 2
            putStrLn "NOPE"
            primeLoop latest'' soFar pFile buffer sList