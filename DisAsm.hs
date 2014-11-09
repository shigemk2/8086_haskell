module DisAsm where

import Data.Bits
import Hex

regs  = [reg8, reg16]

disasm' hex = disasm $ hexStrToList hex

disasm (x:xs) = disasmB (getBits x) xs

disasmB (1,0,0,0,1,0,d,w) xs
    | d == 0    = "mov " ++ rm  ++ "," ++ reg
    | otherwise = "mov " ++ reg ++ "," ++ rm
    where
        (rm, r) = modrm xs
        reg = regs !! w !! r
disasmB (1,0,1,1,w,r,e,g) xs =
    "mov " ++ reg ++ "," ++ imm
    where
        reg = regs !! w !! getReg r e g
        imm = "0x" ++ hex (fromLE (w + 1) xs)

regad = ["bx+si", "bx+di", "bp+si", "bp+di", "si", "di", "bp", "bx"]

modrm (x:xs) = (f mode rm, reg)
    where
        mode =  x `shiftR` 6
        reg  = (x `shiftR` 3) .&. 7
        rm   =  x             .&. 7
        f 0 6  = "[0x" ++ hex (fromLE 2 xs) ++ "]"
        f 0 rm = "[" ++ regad !! rm ++ "]"
        f 1 rm = "[" ++ regad !! rm ++ disp ++ "]"
            where
                disp = disp8 (xs !! 0)
        f 2 rm = "[" ++ regad !! rm ++ disp ++ "]"
            where
                disp = disp16 (fromLE 2 xs)
        f 3 rm = reg16 !! rm

reg16 = ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di"]
reg8  = ["al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"]


getBits :: Int -> (Int,Int,Int,Int,Int,Int,Int,Int)
getBits x = (b 7, b 6, b 5, b 4, b 3, b 2, b 1, b 0)
    where
        b n = (x `shiftR` n) .&. 1

getReg :: Int -> Int -> Int -> Int
getReg r e g =
    (r `shiftL` 2) .|. (e `shiftL` 1) .|. g

disp8 x
    | x < 0x80  = "+0x" ++ hex x
    | otherwise = "-0x" ++ hex (0x100 - x)

disp16 x
    | x < 0x8000  = "+0x" ++ hex x
    | otherwise = "-0x" ++ hex (0x10000 - x)

