module DisAsm where

import Data.Bits
import Hex

regs  = [reg8, reg16]

-- disasm' hex = disasm $ hexStrToList hex
disasm' hex
    | length bin == len = snd asm
    | otherwise         = "length? " ++ show len
    where
        bin = hexStrToList hex
        asm = disasm bin
        len = fst asm

disasm (x:xs) = disasmB (getBits x) xs

-- Immediate to Register/Memory [100010dw][modregr/m]
disasmB (1,0,0,0,1,0,d,w) xs
    | d == 0    = (len + 1, "mov " ++ rm  ++ "," ++ reg)
    | otherwise = (len + 1, "mov " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm False w xs
        reg = regs !! w !! r

-- Immediate to Register [1011wreg][data][data if w=1]
disasmB (1,0,1,1,w,r,e,g) xs =
    (length xs + 1, "mov " ++ reg ++ "," ++ imm)
    -- "mov " ++ reg ++ "," ++ imm
    where
        reg = regs !! w !! getReg r e g
        imm = "0x" ++ hex (fromLE (w + 1) xs)

-- Immediate to Register/Memory [1100011w][mod000r/m][data][data if w=1]
disasmB (1,1,0,0,0,1,1,w) xs =
    (len + w + 2, "mov " ++ rm ++ "," ++ imm)
    where
        (len, rm, r) = modrm True w xs
        imm = "0x" ++ hex (fromLE (w + 1) (drop len xs))

-- Memory to Accumulator [1010000w][addr-low][addr-high]
disasmB (1,0,1,0,0,0,0,w) xs
    | w == 0    = (3, "mov " ++ rm ++ ",[" ++ imm ++ "]")
    | otherwise = (3, "mov " ++ rm ++ ",[" ++ imm ++ "]")
    where
        rm  = regs !! w !! 0
        imm = "0x" ++ hex (fromLE 2 xs)

-- Accumulator to Memory [1010001w][addr-low][addr-high]
disasmB (1,0,1,0,0,0,1,w) xs
    | w == 0    = (3, "mov [" ++ imm ++ "]," ++ rm)
    | otherwise = (3, "mov [" ++ imm ++ "]," ++ rm)
    where
        rm  = regs !! w !! 0
        imm = "0x" ++ hex (fromLE 2 xs)

-- Register/Memory to Segment Register [10001110][mod0reg r/m]
disasmB (1,0,0,0,1,1,1,0) xs =
    (0, "mov " ++ rmseg ++ "," ++ rm)
    where
        (_, rm, r) = modrm False 1 xs
        rmseg = sreg !! r

-- Segment Register to Register/Memory [10001100][mod0reg r/m]
disasmB (1,0,0,0,1,1,0,0) xs =
    (0, "mov " ++ rm ++ "," ++ rmseg)
    where
        (_, rm, r) = modrm False 1 xs
        rmseg = sreg !! r

regad = ["bx+si", "bx+di", "bp+si", "bp+di", "si", "di", "bp", "bx"]

modrm prefix w (x:xs) = (len, s, reg)
    where
        (len, s) = f mode rm
        mode =  x `shiftR` 6
        reg  = (x `shiftR` 3) .&. 7
        rm   =  x             .&. 7
        pfx | prefix && w == 0 = "byte "
            | prefix && w == 1 = "word "
            | otherwise        = ""
        f 0 6  = (3, pfx ++ "[0x" ++ hex (fromLE 2 xs) ++ "]")
        f 0 rm = (1, pfx ++ "[" ++ regad !! rm ++ "]")
        f 1 rm = (2, pfx ++ "[" ++ regad !! rm ++ disp ++ "]")
            where
                disp = disp8 (xs !! 0)
        f 2 rm = (3, pfx ++ "[" ++ regad !! rm ++ disp ++ "]")
            where
                disp = disp16 (fromLE 2 xs)
        f 3 rm = (1, regs !! w !! rm)

reg16 = ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di"]
reg8  = ["al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"]
sreg = ["es", "cs", "ss", "ds"]


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

