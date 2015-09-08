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
    primeLoop pTest pList pFile pTest
        

----------------------------------------------------

makeList :: Integer -> S.Seq Integer -> S.Seq Integer
makeList start is = S.takeWhileL (\x -> x ^ 2 <= start) is

isPrime :: Integer -> S.Seq Integer -> Bool
isPrime start list = not $ F.any ((==) 0 . mod start) list

addPrime :: Integer -> S.Seq Integer -> S.Seq Integer
addPrime int ints = ints S.|> int

primeLoop :: Integer -> S.Seq Integer -> FilePath -> Integer -> IO ()
primeLoop latest soFar pFile buffer = do
    let latest' = if latest `mod` 5 == 0 then latest + 2 else latest
    let shortList = makeList latest' soFar
    if isPrime latest' shortList
        then do
            let soFar' = addPrime latest' soFar
            let latest'' = latest' + 2
            putStrLn $ "\nWE GOT ONE! (" ++ show latest' ++ ")\n"
            if latest > (buffer + 100000)
                then do
                    writeFile pFile $ show $ F.toList soFar'
                    primeLoop latest'' soFar' pFile latest''
                else primeLoop latest'' soFar' pFile buffer
        else do
            let latest'' = latest' + 2
            putStrLn "NOPE"
            primeLoop latest'' soFar pFile buffer