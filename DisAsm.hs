module DisAsm where

import Data.Bits
import Data.Char
import Hex

regs  = [reg8, reg16]

ndisasm ip xs = (len, addr ++ "  " ++ dump ++ "  " ++ snd asm)
    where
        asm  = disasm xs
        len  = fst asm
        addr = upper $ hexn 8 ip
        dump = upper $ listToHexStr list ++ spc
        list = take len xs
        spc  = replicate (16 - len * 2) ' '
        upper s = map toUpper s
        -- upper s = [toUpper ch | ch <- s]

ndisasms _ [] = []
ndisasms ip xs = snd asm : ndisasms (ip + len) (drop len xs)
    where
        asm = ndisasm ip xs
        len = fst asm


disasms [] = []
disasms xs = asm : disasms (drop len xs)
    where
        asm = disasm xs
        len = fst asm

disasms' hex = [snd asm | asm <- disasms $ hexStrToList hex]
-- disasm' hex = disasm $ hexStrToList hex
disasm' hex
    | length bin == len = snd asm
    | otherwise         = "length? " ++ show len
    where
        bin = hexStrToList hex
        asm = disasm bin
        len = fst asm

disasm (x:xs) = disasmB (getBits x) xs

-- mov
-- Immediate to Register/Memory [100010dw][modregr/m]
disasmB (1,0,0,0,1,0,d,w) xs
    | d == 0    = (1 + len, "mov " ++ rm  ++ "," ++ reg)
    | otherwise = (1 + len, "mov " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm False w xs
        reg = regs !! w !! r

-- Immediate to Register [1011wreg][data][data if w=1]
disasmB (1,0,1,1,w,r,e,g) xs =
    (2 + w, "mov " ++ reg ++ "," ++ imm)
    where
        reg = regs !! w !! getReg r e g
        imm = "0x" ++ hex (fromLE (w + 1) xs)

-- Immediate to Register/Memory [1100011w][mod000r/m][data][data if w=1]
disasmB (1,1,0,0,0,1,1,w) xs =
    (1 + len + w + 1, "mov " ++ rm ++ "," ++ imm)
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
    (1 + len, "mov " ++ rmseg ++ "," ++ rm)
    where
        (len, rm, r) = modrm False 1 xs
        rmseg = sreg !! r

-- Segment Register to Register/Memory [10001100][mod0reg r/m]
disasmB (1,0,0,0,1,1,0,0) xs =
    (1 + len, "mov " ++ rm ++ "," ++ rmseg)
    where
        (len, rm, r) = modrm False 1 xs
        rmseg = sreg !! r

-- push
-- inc
-- dec
-- Register/Memory
disasmB (1,1,1,1,1,1,1,1) xs =
    (1 + len, op ++ " " ++ rm)
    where
        op = oprm !! r
        (len, rm, r) = modrm True 1 xs

-- Register
disasmB (0,1,0,1,0,r,e,g) xs =
    (1, "push " ++ reg)
    where
        reg = regs !! 1 !! getReg r e g

-- Segment Register
disasmB (0,0,0,s,r,1,1,0) xs =
    (1, "push " ++ rmseg)
    where
        rmseg = sreg !! getReg 0 s r

-- pop
-- Register/Memory
disasmB (1,0,0,0,1,1,1,1) xs =
    (1 + len, "pop " ++ rm)
    where
        (len, rm, r) = modrm True 1 xs

-- Register
disasmB (0,1,0,1,1,r,e,g) xs =
    (1, "pop " ++ reg)
    where
        reg = regs !! 1 !! getReg r e g

-- Segment Register
disasmB (0,0,0,s,r,1,1,1) xs =
    (1, "pop " ++ rmseg)
    where
        rmseg = sreg !! getReg 0 s r

-- xchg
-- Register/Memory with Register
disasmB (1,0,0,0,0,1,1,w) xs =
    (1 + len, "xchg " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm False w xs
        reg = regs !! w !! r

-- Register with Accumulator
disasmB (1,0,0,1,0,r,e,g) xs
    -- xchg ax,axはなにもしていないのでnop
    | reg == "ax"    = (1, "nop")
    | otherwise      = (1, "xchg ax," ++ reg)
    where
        reg = regs !! 1 !! getReg r e g

-- in
-- Fixed Port
disasmB (1,1,1,0,0,1,0,w) xs
    | w == 0    = (2, "in al," ++ imm)
    | otherwise = (2, "in ax," ++ imm)
    where
        imm = "0x" ++ hex (fromLE 1 xs)

-- in
-- Variable Port
disasmB (1,1,1,0,1,1,0,w) xs
    | w == 0    = (1, "in al,dx")
    | otherwise = (1, "in ax,dx")

-- out
-- Fixed Port
disasmB (1,1,1,0,0,1,1,w) xs
    | w == 0    = (2, "out " ++ imm ++ ",al")
    | otherwise = (2, "out " ++ imm ++ ",ax")
    where
        imm = "0x" ++ hex (fromLE 1 xs)

-- xlat
disasmB (1,1,0,1,0,1,1,1) xs =
    (1, "xlatb")

-- lea
disasmB (1,0,0,0,1,1,0,1) xs =
    (1 + len, "lea " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm False 1 xs
        reg = regs !! 1 !! r

-- lds
disasmB (1,1,0,0,0,1,0,1) xs =
    (1 + len, "lds " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm False 1 xs
        reg = regs !! 1 !! r

-- les
disasmB (1,1,0,0,0,1,0,0) xs =
    (1 + len, "les " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm False 1 xs
        reg = regs !! 1 !! r

-- lahf
disasmB (1,0,0,1,1,1,1,1) xs =
    (1, "lahf")

-- sahf
disasmB (1,0,0,1,1,1,1,0) xs =
    (1, "sahf")

-- pushf
disasmB (1,0,0,1,1,1,0,0) xs =
    (1, "pushfw")

-- popf
disasmB (1,0,0,1,1,1,0,1) xs =
    (1, "popfw")

-- add
-- Reg./Memory with Register to Either
disasmB (0,0,0,0,0,0,d,w) xs
    | d == 0    = (1 + len, "add " ++ rm  ++ "," ++ reg)
    | otherwise = (1 + len, "add " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm False w xs
        reg = regs !! w !! r

-- Immediate to Register/MemoryはdisasmB (0,0,0,1,0,1,0,w)で実装

-- Immediate to Accumulator
disasmB (0,0,0,0,0,1,0,w) xs
    | w == 0    = (2, "add al," ++ imm)
    | otherwise = (3, "add ax," ++ imm)
    where
        imm = "0x" ++ hex (fromLE (1 + w) xs)

-- adc
-- Reg./Memory with Register to Either
disasmB (0,0,0,1,0,0,d,w) xs
    | d == 0    = (1 + len, "adc " ++ rm  ++ "," ++ reg)
    | otherwise = (1 + len, "adc " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm False w xs
        reg = regs !! w !! r

-- Immediate to Register/MemoryはdisasmB (0,0,0,1,0,1,0,w)で実装

-- Immediate to Accumulator
disasmB (0,0,0,1,0,1,0,w) xs
    | w == 0    = (2, "adc al," ++ imm)
    | otherwise = (3, "adc ax," ++ imm)
    where
        imm = "0x" ++ hex (fromLE (1 + w) xs)

-- add adc Immediate to Register/Memory
-- sub sbb Immediate from Register/Memory
-- cmp Immediate with Register/Memory
disasmB (1,0,0,0,0,0,s,w) xs
    | getReg 0 s w == 2    = (1, "db 0x82")
    | getReg 0 s w == 3    = (1 + len + 1, op ++ " " ++ rm ++ ",byte +" ++ imms)
    -- s w = 10 のときは欠番
    | otherwise = (1 + len + w + 1, op ++ " " ++ rm ++ "," ++ imm)
    where
        op = opirm !! r
        (len, rm, r) = modrm True w xs
        imms = "0x" ++ hex (fromLE 1 (drop len xs))
        imm  = "0x" ++ hex (fromLE (w + 1) (drop len xs))

-- inc
-- dec
-- Register/Memory
-- w == 1はpushのと被るのでpushで場合分けしています
disasmB (1,1,1,1,1,1,1,w) xs
    | w == 0    = (1 + len, op ++ " " ++ rm)
    | otherwise = (1 + len, op ++ " " ++ rm)
    where
        op = oprm !! r
        (len, rm, r) = modrm True w xs

-- Register
disasmB (0,1,0,0,0,r,e,g) xs =
    (1, "inc " ++ reg)
    where
        reg = regs !! 1 !! getReg r e g

-- aaa
disasmB (0,0,1,1,0,1,1,1) xs =
    (1, "aaa")

-- daa
disasmB (0,0,1,0,0,1,1,1) xs =
    (1, "daa")

-- sub
-- Reg./Memory and Register to Either
disasmB (0,0,1,0,1,0,d,w) xs
    | d == 0    = (1 + len, "sub " ++ rm  ++ "," ++ reg)
    | otherwise = (1 + len, "sub " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm False w xs
        reg = regs !! w !! r

-- Immediate from Register/MemoryはdisasmB (0,0,0,1,0,1,0,w)で実装

-- Immediate from Accumulator
disasmB (0,0,1,0,1,1,0,w) xs
    | w == 0    = (2, "sub al," ++ imm)
    | otherwise = (3, "sub ax," ++ imm)
    where
        imm = "0x" ++ hex (fromLE (1 + w) xs)

-- sbb
-- Reg./Memory and Register to Either
disasmB (0,0,0,1,1,0,d,w) xs
    | d == 0    = (1 + len, "sbb " ++ rm  ++ "," ++ reg)
    | otherwise = (1 + len, "sbb " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm False w xs
        reg = regs !! w !! r

-- Immediate from Register/MemoryはdisasmB (0,0,0,1,0,1,0,w)で実装

-- Immediate to Accumulator
disasmB (0,0,0,1,1,1,0,w) xs
    | w == 0    = (2, "sbb al," ++ imm)
    | otherwise = (3, "sbb ax," ++ imm)
    where
        imm = "0x" ++ hex (fromLE (1 + w) xs)

-- dec
-- Register/Memory
-- inc命令のdisasmB (1,1,1,1,1,1,1,w) xsと統合

-- Register
disasmB (0,1,0,0,1,r,e,g) xs =
    (1, "dec " ++ reg)
    where
        reg = regs !! 1 !! getReg r e g

-- neg
disasmB (1,1,1,1,0,1,1,w) xs
    | r == 3 = (1 + len, "neg " ++ rm)
    where
        (len, rm, r) = modrm True w xs

-- cmp
-- Register/Memory and Register
disasmB (0,0,1,1,1,0,d,w) xs
    | d == 0    = (1 + len, "cmp " ++ rm  ++ "," ++ reg)
    | otherwise = (1 + len, "cmp " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm False w xs
        reg = regs !! w !! r

-- Immediate from Register/MemoryはdisasmB (0,0,0,1,0,1,0,w)で実装

-- Immediate with Accumulator
disasmB (0,0,1,1,1,1,0,w) xs
    | w == 0    = (2, "cmp al," ++ imm)
    | otherwise = (3, "cmp ax," ++ imm)
    where
        imm = "0x" ++ hex (fromLE (1 + w) xs)

-- aas
disasmB (0,0,1,1,1,1,1,1) xs =
    (1, "aas")

-- das
disasmB (0,0,1,0,1,1,1,1) xs =
    (1, "das")

-- mul
disasmB (1,1,1,1,0,1,1,w) xs
    | r == 4 = (1 + len, "mul " ++ rm)
    where
        (len, rm, r) = modrm True w xs

regad = ["bx+si", "bx+di", "bp+si", "bp+di", "si", "di", "bp", "bx"]
-- opecode when [Immediate to Register/Memory|Immediate from Register/Memory]
opirm = ["add", "", "adc", "sbb", "", "sub", "", "cmp"]
-- opecode when Register/Memory
oprm = ["inc", "dec", "", "", "", "", "push"]

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

