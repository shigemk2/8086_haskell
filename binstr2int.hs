module Main where

import Test.HUnit
import System.IO

binToInt '0' = 0
binToInt '1' = 1

-- 2進数の文字列を数値に直す
-- 一文字一文字見るところで再帰を引っ掛けている
-- where部分が再帰部分
-- reverseは最初だけ
-- 文字をひっくり返して2でかける
binStrToInt bin = f (reverse bin)
    where
        f ""     = 0
        f (x:xs) = (binToInt x) + 2 * (f xs)

tests = TestList
    [ "reverse"       ~: reverse     "11001"  ~?= "10011"
    , "binStrToInt 1" ~: binStrToInt "101"    ~?= 5
    , "binStrToInt 2" ~: binStrToInt "11001"  ~?= 25
    , "binStrToInt 3" ~: binStrToInt "11111"  ~?= 31
    , "binStrToInt 3" ~: binStrToInt "110010" ~?= 50
    ]

main = do
    runTestText (putTextToHandle stderr False) tests
