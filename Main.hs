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

-- メモリをシミュレートするためバイト区切りのリスト
hex x
    | x1 == 0   = x2
    | otherwise = hex x1 ++ x2
    where
        x1 = x `div` 16
        x2 = [intToDigit (x `mod` 16)]

hexn n x
    | r < 0     = drop (-r) x'
    | otherwise = replicate r '0' ++ x'
    where
        x' = hex x
        r  = n - length x'

hexStrToList ""       = []
hexStrToList (h:l:xs) = hexStrToInt [h, l] : hexStrToList xs

listToHexStr []     = ""
listToHexStr (x:xs) = hexn 2 x ++ listToHexStr xs

-- バイトオーダー
-- メモリはバイトごとに区切られています。1バイトに収まりきらない数値は分割して格納します
-- 分割した際に逆順に並べ替える方式をリトルエンディアンと呼びます。並べ替えない方式はビッグエンディアン
-- 8086以降はリトルエンディアン
-- バイトごとに手で区切る
-- バイトオーダーを考えるのは分割後です。分割前のバイトオーダーは考えません
-- 数値→リトルエンディアン
toLE 0 _ = []
toLE n x = x `mod` 0x100 : toLE (n - 1) (x `div` 0x100)
-- リトルエンディアン→数値
fromLE 0 _      = 0
fromLE n (x:xs) = x + 0x100 * fromLE (n - 1) xs
-- 数値→ビッグエンディアン
toBE 0 _ = []
toBE n x = x `div` (0x100 ^ (n - 1)) : toBE (n - 1) (x `mod` (0x100 ^ (n - 1)))

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
        , "replicate" ~: replicate 5 'a' ~?= "aaaaa"
        , "intToDigit" ~: intToDigit 10  ~?= 'a'
        , "hex 1" ~: hex 256   ~?= "100"
        , "hex 2" ~: hex 65535 ~?= "ffff"
        , "hexn 1" ~: hexn 2 1     ~?= "01"
        , "hexn 2" ~: hexn 2 255   ~?= "ff"
        , "hexn 3" ~: hexn 8 65535 ~?= "0000ffff"
        , "hexn 4" ~: hexn 2 256   ~?= "00"
        , "hexStrToList 1" ~: hexStrToList "123456" ~?= [0x12, 0x34, 0x56]
        , "hexStrToList 2" ~: hexStrToList "010203" ~?= [1, 2, 3]
        , "listToHexStr 1" ~: listToHexStr [0x12, 0x34, 0x56] ~?= "123456"
        , "listToHexStr 2" ~: listToHexStr [1, 2, 3]          ~?= "010203"
        , "toLE 1" ~: toLE 2 1          ~?= [1, 0]
        -- 桁数制限ではみ出した奴は消える
        , "toLE 2" ~: toLE 2 0x10000    ~?= [0, 0]
        , "toLE 3" ~: toLE 4 0x12345678 ~?= [0x78, 0x56, 0x34, 0x12]
        , "fromLE 1" ~: fromLE 2 [0, 1]                   ~?= 0x100
        , "fromLE 2" ~: fromLE 2 [0x78, 0x56, 0x34, 0x12] ~?= 0x5678
        , "fromLE 3" ~: fromLE 4 [0x78, 0x56, 0x34, 0x12] ~?= 0x12345678
        , "toBE 1" ~: toBE 2 1          ~?= [0, 1]
        , "toBE 2" ~: toBE 2 0x10000    ~?= [0, 0]
        , "toBE 3" ~: toBE 4 0x12345678 ~?= [0x12, 0x34, 0x56, 0x78]
        ]

main = do
    runTestText (putTextToHandle stderr False) tests
