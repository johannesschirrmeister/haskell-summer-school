module Validation where

import Data.Char
import Data.List.Split

-- Ex. 1
toDigits :: Integer -> [Integer]
toDigits n = map (toInteger . digitToInt) (show n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- Ex. 2
doubleSecond :: (Num a) => [a] -> [a]
doubleSecond [] = []
doubleSecond (x:[]) = (x:[])
doubleSecond (x:xs:xss) = x : xs * 2 : doubleSecond xss

-- Ex. 3
splitDoubleDigits :: [Integer] -> [Integer]
splitDoubleDigits [] = []
splitDoubleDigits (x:xs) = if x >= 10 
        then (toDigits x) ++ splitDoubleDigits xs
        else x : splitDoubleDigits xs

sumDigits :: [Integer] -> Integer
sumDigits d = foldr (+) 0 (splitDoubleDigits d) 

-- Ex. 4
validate :: Integer -> Bool
validate n = (sumDigits (splitDoubleDigits $ doubleSecond $ toDigitsRev n) `mod` 10) == 0

-- Ex. 5
filterDigits :: String -> String
filterDigits s = filter isDigit s

readCC :: String -> Integer
readCC s = read (filterDigits s) :: Integer

-- Ex. 6
addPrecedingZeros :: String -> String
addPrecedingZeros s = if length s < 16 then addPrecedingZeros ('0':s) else s

addSpaces :: String -> String
addSpaces s = unwords (chunksOf 4 s)

showCC :: Integer -> String
showCC n = addSpaces $ addPrecedingZeros (show n)        

-- Ex. 7
lookupIssuer :: String -> Integer -> IO String
lookupIssuer = undefined

-- Ex. 8
checkCC :: String -> IO ()
checkCC = undefined

-- Ex. 9: BONUS! Leave undefined if you do not do it.
toDigitsRevG :: (Integral a) => a -> a -> [a]
toDigitsRevG = undefined