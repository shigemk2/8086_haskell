module Main where

import Test.HUnit
import System.IO
import Data.Char

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

hexStrToInt hex = f (reverse hex)
    where
        f ""     = 0
        f (x:xs) = (digitToInt x) + 16 * (f xs)

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
        , "digitToInt" ~: digitToInt 'a' ~?= 10
        , "hexStrToInt 1" ~: hexStrToInt "100"  ~?= 256
        , "hexStrToInt 2" ~: hexStrToInt "ffff" ~?= 65535
        ]

main = do
    runTestText (putTextToHandle stderr False) tests
