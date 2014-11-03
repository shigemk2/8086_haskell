module Hex where
import Test.HUnit
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

toLE 0 _ = []
toLE n x = x `mod` 0x100 : toLE (n - 1) (x `div` 0x100)
fromLE 0 _      = 0
fromLE n (x:xs) = x + 0x100 * fromLE (n - 1) xs
toBE 0 _ = []
toBE n x = x `div` (0x100 ^ (n - 1)) `mod` 0x100 : toBE (n - 1) x
fromBE 0 _ = 0
fromBE n (x:xs) = x * 0x100^(n - 1) + fromBE (n - 1) xs

