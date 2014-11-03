module Main where

import Test.HUnit
import System.IO

binToInt '0' = 0
binToInt '1' = 1

binStrToInt bin = f (reverse bin)
    where
        f ""     = 0
        f (x:xs) = (binToInt x) + 2 * (f xs)

intToBin 0 = '0'
intToBin 1 = '1'

bin x
    | x1 == 0   = x2
    | otherwise = bin x1 ++ x2
    where
      x1 = x `div` 2
      x2 = [intToBin $ x `mod` 2]

tests = TestList
        [ "reverse"       ~: reverse     "11001"  ~?= "10011" 
        , "binStrToInt 5" ~: binStrToInt "101"    ~?= 5
        , "binStrToInt 25" ~: binStrToInt "11001"  ~?= 25
        , "binStrToInt 31" ~: binStrToInt "11111"  ~?= 31
        , "binStrToInt 350" ~: binStrToInt "110010" ~?= 50
        , "div 1"         ~: 1 `div` 2     ~?= 0
        , "bin 0" ~: bin 0 ~?= "0"
        , "bin 1" ~: bin 1 ~?= "1"
        , "bin 5" ~: bin 5 ~?= "101"
        , "bin 25" ~: bin 25 ~?= "11001"
        , "bin 31" ~: bin 31 ~?= "11111"
        , "bin 50" ~: bin 50 ~?= "110010"
        ]

main = do
    runTestText (putTextToHandle stderr False) tests
