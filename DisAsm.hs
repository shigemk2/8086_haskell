module DisAsm where

import Data.Bits
import Data.Char
import Hex

regs  = [reg8, reg16]

ndisasm ip xs = (len, addr ++ "  " ++ dump ++ "  " ++ snd asm)
    where
        asm  = disasm ip xs
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


disasms _ [] = []
disasms ip xs = asm : disasms ip (drop len xs)
    where
        asm = disasm ip xs
        len = fst asm

disasms' hex = [snd asm | asm <- disasms 0 $ hexStrToList hex]
-- disasm' hex = disasm $ hexStrToList hex
disasm' hex
    | length bin == len = snd asm
    | otherwise         = "length? " ++ show len
    where
        bin = hexStrToList hex
        asm = disasm 0 bin
        len = fst asm

disasm ip (x:xs) = disasmB ip (getBits x) xs

-- mov
-- Immediate to Register/Memory [100010dw][modregr/m]
disasmB _ (1,0,0,0,1,0,d,w) xs
    | d == 0    = (1 + len, "mov " ++ rm  ++ "," ++ reg)
    | otherwise = (1 + len, "mov " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm False w xs
        reg = regs !! w !! r

-- Immediate to Register [1011wreg][data][data if w=1]
disasmB _ (1,0,1,1,w,r,e,g) xs =
    (2 + w, "mov " ++ reg ++ "," ++ imm)
    where
        reg = regs !! w !! getReg r e g
        imm = "0x" ++ hex (fromLE (w + 1) xs)

-- Immediate to Register/Memory [1100011w][mod000r/m][data][data if w=1]
disasmB _ (1,1,0,0,0,1,1,w) xs =
    (1 + len + w + 1, "mov " ++ rm ++ "," ++ imm)
    where
        (len, rm, r) = modrm True w xs
        imm = "0x" ++ hex (fromLE (w + 1) (drop len xs))

-- Memory to Accumulator [1010000w][addr-low][addr-high]
disasmB _ (1,0,1,0,0,0,0,w) xs
    | w == 0    = (3, "mov " ++ rm ++ ",[" ++ imm ++ "]")
    | otherwise = (3, "mov " ++ rm ++ ",[" ++ imm ++ "]")
    where
        rm  = regs !! w !! 0
        imm = "0x" ++ hex (fromLE 2 xs)

-- Accumulator to Memory [1010001w][addr-low][addr-high]
disasmB _ (1,0,1,0,0,0,1,w) xs
    | w == 0    = (3, "mov [" ++ imm ++ "]," ++ rm)
    | otherwise = (3, "mov [" ++ imm ++ "]," ++ rm)
    where
        rm  = regs !! w !! 0
        imm = "0x" ++ hex (fromLE 2 xs)

-- Register/Memory to Segment Register [10001110][mod0reg r/m]
disasmB _ (1,0,0,0,1,1,1,0) xs =
    (1 + len, "mov " ++ rmseg ++ "," ++ rm)
    where
        (len, rm, r) = modrm False 1 xs
        rmseg = sreg !! r

-- Segment Register to Register/Memory [10001100][mod0reg r/m]
disasmB _ (1,0,0,0,1,1,0,0) xs =
    (1 + len, "mov " ++ rm ++ "," ++ rmseg)
    where
        (len, rm, r) = modrm False 1 xs
        rmseg = sreg !! r

-- push
-- Register/Memory
disasmB _ (1,1,1,1,1,1,1,1) xs
    | r == 6 = (1 + len, "push " ++ rm)
    where
        (len, rm, r) = modrm True 1 xs

-- Register
disasmB _ (0,1,0,1,0,r,e,g) xs =
    (1, "push " ++ reg)
    where
        reg = regs !! 1 !! getReg r e g

-- Segment Register
disasmB _ (0,0,0,s,r,1,1,0) xs =
    (1, "push " ++ rmseg)
    where
        rmseg = sreg !! getReg 0 s r

-- pop
-- Register/Memory
disasmB _ (1,0,0,0,1,1,1,1) xs =
    (1 + len, "pop " ++ rm)
    where
        (len, rm, r) = modrm True 1 xs

-- Register
disasmB _ (0,1,0,1,1,r,e,g) xs =
    (1, "pop " ++ reg)
    where
        reg = regs !! 1 !! getReg r e g

-- Segment Register
disasmB _ (0,0,0,s,r,1,1,1) xs =
    (1, "pop " ++ rmseg)
    where
        rmseg = sreg !! getReg 0 s r

-- xchg
-- Register/Memory with Register
disasmB _ (1,0,0,0,0,1,1,w) xs =
    (1 + len, "xchg " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm False w xs
        reg = regs !! w !! r

-- Register with Accumulator
disasmB _ (1,0,0,1,0,r,e,g) xs
    -- xchg ax,axはなにもしていないのでnop
    | reg == "ax"    = (1, "nop")
    | otherwise      = (1, "xchg ax," ++ reg)
    where
        reg = regs !! 1 !! getReg r e g

-- in
-- Fixed Port
disasmB _ (1,1,1,0,0,1,0,w) xs
    | w == 0    = (2, "in al," ++ imm)
    | otherwise = (2, "in ax," ++ imm)
    where
        imm = "0x" ++ hex (fromLE 1 xs)

-- in
-- Variable Port
disasmB _ (1,1,1,0,1,1,0,w) xs
    | w == 0    = (1, "in al,dx")
    | otherwise = (1, "in ax,dx")

-- out
-- Fixed Port
disasmB _ (1,1,1,0,0,1,1,w) xs
    | w == 0    = (2, "out " ++ imm ++ ",al")
    | otherwise = (2, "out " ++ imm ++ ",ax")
    where
        imm = "0x" ++ hex (fromLE 1 xs)

-- xlat
disasmB _ (1,1,0,1,0,1,1,1) xs =
    (1, "xlatb")

-- lea
disasmB _ (1,0,0,0,1,1,0,1) xs =
    (1 + len, "lea " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm False 1 xs
        reg = regs !! 1 !! r

-- lds
disasmB _ (1,1,0,0,0,1,0,1) xs =
    (1 + len, "lds " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm False 1 xs
        reg = regs !! 1 !! r

-- les
disasmB _ (1,1,0,0,0,1,0,0) xs =
    (1 + len, "les " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm False 1 xs
        reg = regs !! 1 !! r

-- lahf
disasmB _ (1,0,0,1,1,1,1,1) xs =
    (1, "lahf")

-- sahf
disasmB _ (1,0,0,1,1,1,1,0) xs =
    (1, "sahf")

-- pushf
disasmB _ (1,0,0,1,1,1,0,0) xs =
    (1, "pushfw")

-- popf
disasmB _ (1,0,0,1,1,1,0,1) xs =
    (1, "popfw")

-- add
-- Reg./Memory with Register to Either
disasmB _ (0,0,0,0,0,0,d,w) xs
    | d == 0    = (1 + len, "add " ++ rm  ++ "," ++ reg)
    | otherwise = (1 + len, "add " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm False w xs
        reg = regs !! w !! r

-- Immediate to Register/Memory
disasmB _ (1,0,0,0,0,0,s,w) xs
    -- s w = 10 のときは欠番
    | getReg 0 s w == 2           = (1, "db 0x82")
    | getReg 0 s w == 3 && r == 0 = (1 + len + 1, "add " ++ rm ++ ",byte +" ++ imms)
    |                      r == 0 = (1 + len + w + 1, "add " ++ rm ++ "," ++ imm)
    where
        (len, rm, r) = modrm True w xs
        imms = "0x" ++ hex (fromLE 1 (drop len xs))
        imm  = "0x" ++ hex (fromLE (w + 1) (drop len xs))

-- Immediate to Accumulator
disasmB _ (0,0,0,0,0,1,0,w) xs
    | w == 0    = (2, "add al," ++ imm)
    | otherwise = (3, "add ax," ++ imm)
    where
        imm = "0x" ++ hex (fromLE (1 + w) xs)

-- adc
-- Reg./Memory with Register to Either
disasmB _ (0,0,0,1,0,0,d,w) xs
    | d == 0    = (1 + len, "adc " ++ rm  ++ "," ++ reg)
    | otherwise = (1 + len, "adc " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm False w xs
        reg = regs !! w !! r

-- Immediate to Register/Memory
disasmB _ (1,0,0,0,0,0,s,w) xs
    -- s w = 10 のときは欠番
    | getReg 0 s w == 2           = (1, "db 0x82")
    | getReg 0 s w == 3 && r == 2 = (1 + len + 1, "adc " ++ rm ++ ",byte +" ++ imms)
    |                      r == 2 = (1 + len + w + 1, "adc " ++ rm ++ "," ++ imm)
    where
        (len, rm, r) = modrm True w xs
        imms = "0x" ++ hex (fromLE 1 (drop len xs))
        imm  = "0x" ++ hex (fromLE (w + 1) (drop len xs))

-- Immediate to Accumulator
disasmB _ (0,0,0,1,0,1,0,w) xs
    | w == 0    = (2, "adc al," ++ imm)
    | otherwise = (3, "adc ax," ++ imm)
    where
        imm = "0x" ++ hex (fromLE (1 + w) xs)

-- inc
-- Register/Memory
disasmB _ (1,1,1,1,1,1,1,w) xs
    | w == 0 && r == 0 = (1 + len, "inc " ++ rm)
    | w == 1 && r == 0 = (1 + len, "inc " ++ rm)
    where
        (len, rm, r) = modrm True w xs

-- Register
disasmB _ (0,1,0,0,0,r,e,g) xs =
    (1, "inc " ++ reg)
    where
        reg = regs !! 1 !! getReg r e g

-- aaa
disasmB _ (0,0,1,1,0,1,1,1) xs =
    (1, "aaa")

-- daa
disasmB _ (0,0,1,0,0,1,1,1) xs =
    (1, "daa")

-- sub
-- Reg./Memory and Register to Either
disasmB _ (0,0,1,0,1,0,d,w) xs
    | d == 0    = (1 + len, "sub " ++ rm  ++ "," ++ reg)
    | otherwise = (1 + len, "sub " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm False w xs
        reg = regs !! w !! r

-- Immediate to Register/Memory
disasmB _ (1,0,0,0,0,0,s,w) xs
    -- s w = 10 のときは欠番
    | getReg 0 s w == 2           = (1, "db 0x82")
    | getReg 0 s w == 3 && r == 5 = (1 + len + 1, "sub " ++ rm ++ ",byte +" ++ imms)
    |                      r == 5 = (1 + len + w + 1, "sub " ++ rm ++ "," ++ imm)
    where
        (len, rm, r) = modrm True w xs
        imms = "0x" ++ hex (fromLE 1 (drop len xs))
        imm  = "0x" ++ hex (fromLE (w + 1) (drop len xs))

-- Immediate from Accumulator
disasmB _ (0,0,1,0,1,1,0,w) xs
    | w == 0    = (2, "sub al," ++ imm)
    | otherwise = (3, "sub ax," ++ imm)
    where
        imm = "0x" ++ hex (fromLE (1 + w) xs)

-- sbb
-- Reg./Memory and Register to Either
disasmB _ (0,0,0,1,1,0,d,w) xs
    | d == 0    = (1 + len, "sbb " ++ rm  ++ "," ++ reg)
    | otherwise = (1 + len, "sbb " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm False w xs
        reg = regs !! w !! r

-- Immediate to Register/Memory
disasmB _ (1,0,0,0,0,0,s,w) xs
    -- s w = 10 のときは欠番
    | getReg 0 s w == 2           = (1, "db 0x82")
    | getReg 0 s w == 3 && r == 3 = (1 + len + 1, "sbb " ++ rm ++ ",byte +" ++ imms)
    |                      r == 3 = (1 + len + w + 1, "sbb " ++ rm ++ "," ++ imm)
    where
        (len, rm, r) = modrm True w xs
        imms = "0x" ++ hex (fromLE 1 (drop len xs))
        imm  = "0x" ++ hex (fromLE (w + 1) (drop len xs))

-- Immediate to Accumulator
disasmB _ (0,0,0,1,1,1,0,w) xs
    | w == 0    = (2, "sbb al," ++ imm)
    | otherwise = (3, "sbb ax," ++ imm)
    where
        imm = "0x" ++ hex (fromLE (1 + w) xs)

-- dec
-- Register/Memory
disasmB _ (1,1,1,1,1,1,1,w) xs
    | w == 0 && r == 1 = (1 + len, "dec " ++ rm)
    | w == 1 && r == 1 = (1 + len, "dec " ++ rm)
    where
        (len, rm, r) = modrm True w xs

-- Register
disasmB _ (0,1,0,0,1,r,e,g) xs =
    (1, "dec " ++ reg)
    where
        reg = regs !! 1 !! getReg r e g

-- neg
disasmB _ (1,1,1,1,0,1,1,w) xs
    | r == 3 = (1 + len, "neg " ++ rm)
    where
        (len, rm, r) = modrm True w xs

-- cmp
-- Register/Memory and Register
disasmB _ (0,0,1,1,1,0,d,w) xs
    | d == 0    = (1 + len, "cmp " ++ rm  ++ "," ++ reg)
    | otherwise = (1 + len, "cmp " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm False w xs
        reg = regs !! w !! r

-- Immediate to Register/Memory
disasmB _ (1,0,0,0,0,0,s,w) xs
    -- s w = 10 のときは欠番
    | getReg 0 s w == 2           = (1, "db 0x82")
    | getReg 0 s w == 3 && r == 7 = (1 + len + 1, "cmp " ++ rm ++ ",byte +" ++ imms)
    |                      r == 7 = (1 + len + w + 1, "cmp " ++ rm ++ "," ++ imm)
    where
        (len, rm, r) = modrm True w xs
        imms = "0x" ++ hex (fromLE 1 (drop len xs))
        imm  = "0x" ++ hex (fromLE (w + 1) (drop len xs))

-- Immediate with Accumulator
disasmB _ (0,0,1,1,1,1,0,w) xs
    | w == 0    = (2, "cmp al," ++ imm)
    | otherwise = (3, "cmp ax," ++ imm)
    where
        imm = "0x" ++ hex (fromLE (1 + w) xs)

-- aas
disasmB _ (0,0,1,1,1,1,1,1) xs =
    (1, "aas")

-- das
disasmB _ (0,0,1,0,1,1,1,1) xs =
    (1, "das")

-- mul
disasmB _ (1,1,1,1,0,1,1,w) xs
    | r == 4 = (1 + len, "mul " ++ rm)
    where
        (len, rm, r) = modrm True w xs

-- imul
disasmB _ (1,1,1,1,0,1,1,w) xs
    | r == 5 = (1 + len, "imul " ++ rm)
    where
        (len, rm, r) = modrm True w xs

-- aam
disasmB _ (1,1,0,1,0,1,0,0) xs
    | getBits (head xs) == (0,0,0,0,1,0,1,0) = (2, "aam")
    -- | xs == [0x0a] = (2, "aam")

-- div
disasmB _ (1,1,1,1,0,1,1,w) xs
    | r == 6 = (1 + len, "div " ++ rm)
    where
        (len, rm, r) = modrm True w xs

-- idiv
disasmB _ (1,1,1,1,0,1,1,w) xs
    | r == 7 = (1 + len, "idiv " ++ rm)
    where
        (len, rm, r) = modrm True w xs

-- aad
disasmB _ (1,1,0,1,0,1,0,1) xs
    | getBits (head xs) == (0,0,0,0,1,0,1,0) = (2, "aad")

-- cbw
disasmB _ (1,0,0,1,1,0,0,0) xs =
    (1, "cbw")

-- cwd
disasmB _ (1,0,0,1,1,0,0,1) xs =
    (1, "cwd")

-- not
disasmB _ (1,1,1,1,0,1,1,w) xs
    | r == 2 = (1 + len, "not " ++ rm)
    where
        (len, rm, r) = modrm True w xs

-- shl/sal
disasmB _ (1,1,0,1,0,0,v,w) xs
    | r == 4 && v == 0 = (len + 1, "shl " ++ rm ++ ",1")
    | r == 4 && v == 1 = (len + 1, "shl " ++ rm ++ ",cl")
    where
        (len, rm, r) = modrm True w xs

-- shr
disasmB _ (1,1,0,1,0,0,v,w) xs
    | r == 5 && v == 0 = (len + 1, "shr " ++ rm ++ ",1")
    | r == 5 && v == 1 = (len + 1, "shr " ++ rm ++ ",cl")
    where
        (len, rm, r) = modrm True w xs

-- sar
disasmB _ (1,1,0,1,0,0,v,w) xs
    | r == 7 && v == 0 = (len + 1, "sar " ++ rm ++ ",1")
    | r == 7 && v == 1 = (len + 1, "sar " ++ rm ++ ",cl")
    where
        (len, rm, r) = modrm True w xs

-- rol
disasmB _ (1,1,0,1,0,0,v,w) xs
    | r == 0 && v == 0 = (len + 1, "rol " ++ rm ++ ",1")
    | r == 0 && v == 1 = (len + 1, "rol " ++ rm ++ ",cl")
    where
        (len, rm, r) = modrm True w xs

-- ror
disasmB _ (1,1,0,1,0,0,v,w) xs
    | r == 1 && v == 0 = (len + 1, "ror " ++ rm ++ ",1")
    | r == 1 && v == 1 = (len + 1, "ror " ++ rm ++ ",cl")
    where
        (len, rm, r) = modrm True w xs

-- rcl
disasmB _ (1,1,0,1,0,0,v,w) xs
    | r == 2 && v == 0 = (len + 1, "rcl " ++ rm ++ ",1")
    | r == 2 && v == 1 = (len + 1, "rcl " ++ rm ++ ",cl")
    where
        (len, rm, r) = modrm True w xs

-- rcr
disasmB _ (1,1,0,1,0,0,v,w) xs
    | r == 3 && v == 0 = (len + 1, "rcr " ++ rm ++ ",1")
    | r == 3 && v == 1 = (len + 1, "rcr " ++ rm ++ ",cl")
    where
        (len, rm, r) = modrm True w xs

-- and
-- Reg./Memory and Register to Either
disasmB _ (0,0,1,0,0,0,d,w) xs
    | d == 0    = (1 + len, "and " ++ rm  ++ "," ++ reg)
    | otherwise = (1 + len, "and " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm False w xs
        reg = regs !! w !! r

-- Immediate to Register/Memory
disasmB _ (1,0,0,0,0,0,0,w) xs
    | w == 0 && r == 4 = (2 + len, "and " ++ rm ++ "," ++ imm)
    | w == 1 && r == 4 = (2 + 1 + len, "and " ++ rm ++ "," ++ imm)
    where
        (len, rm, r) = modrm True w xs
        imm = "0x" ++ hex (fromLE (w + 1) (drop len xs))

-- Immediate to Accumulator
disasmB _ (0,0,1,0,0,1,0,w) xs
    | w == 0    = (2, "and al," ++ imm)
    | otherwise = (3, "and ax," ++ imm)
    where
        imm = "0x" ++ hex (fromLE (1 + w) xs)

-- test
-- Register/Memory and Register
disasmB _ (1,0,0,0,0,1,0,w) xs
    | w == 0    = (1 + len, "test " ++ rm ++ "," ++ reg)
    | otherwise = (1 + len, "test " ++ rm ++ "," ++ reg)
    where
        (len, rm, r) = modrm False w xs
        reg = regs !! w !! r

-- Immediate Data and Register/Memory
disasmB _ (1,1,1,1,0,1,1,w) xs
    | r == 0 = (2 + len + w, "test " ++ rm ++ "," ++ imm)
    where
        (len, rm, r) = modrm True w xs
        imm = "0x" ++ hex (fromLE (1 + w) (drop len xs))

-- Immediate Data and Accumulator
disasmB _ (1,0,1,0,1,0,0,w) xs
    | w == 0    = (2 + w, "test " ++ "al," ++ imm)
    | otherwise = (2 + w, "test " ++ "ax," ++ imm)
    where
        imm = "0x" ++ hex (fromLE (1 + w) xs)

-- or
-- Reg./Memory and Register to Either
disasmB _ (0,0,0,0,1,0,d,w) xs
    | d == 0    = (1 + len, "or " ++ rm  ++ "," ++ reg)
    | otherwise = (1 + len, "or " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm False w xs
        reg = regs !! w !! r

-- Immediate to Register/Memory
disasmB _ (1,0,0,0,0,0,0,w) xs
    | w == 0 && r == 1 = (2 + len, "or " ++ rm ++ "," ++ imm)
    | w == 1 && r == 1 = (2 + 1 + len, "or " ++ rm ++ "," ++ imm)
    where
        (len, rm, r) = modrm True w xs
        imm = "0x" ++ hex (fromLE (w + 1) (drop len xs))

-- Immediate to Accumulator
disasmB _ (0,0,0,0,1,1,0,w) xs
    | w == 0    = (2, "or al," ++ imm)
    | otherwise = (3, "or ax," ++ imm)
    where
        imm = "0x" ++ hex (fromLE (1 + w) xs)

-- xor
-- Reg./Memory and Register to Either
disasmB _ (0,0,1,1,0,0,d,w) xs
    | d == 0    = (1 + len, "xor " ++ rm  ++ "," ++ reg)
    | otherwise = (1 + len, "xor " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm False w xs
        reg = regs !! w !! r

-- Immediate to Register/Memory
disasmB _ (1,0,0,0,0,0,0,w) xs
    | w == 0 && r == 6 = (2 + len, "xor " ++ rm ++ "," ++ imm)
    | w == 1 && r == 6 = (2 + 1 + len, "xor " ++ rm ++ "," ++ imm)
    where
        (len, rm, r) = modrm True w xs
        imm = "0x" ++ hex (fromLE (w + 1) (drop len xs))

-- Immediate to Accumulator
disasmB _ (0,0,1,1,0,1,0,w) xs
    | w == 0    = (2, "xor al," ++ imm)
    | otherwise = (3, "xor ax," ++ imm)
    where
        imm = "0x" ++ hex (fromLE (1 + w) xs)

-- rep
disasmB _ (1,1,1,1,0,0,1,z) xs
    | z == 0    = (1, "repne")
    | otherwise = (1, "rep")

-- movs
disasmB _ (1,0,1,0,0,1,0,w) xs
    | w == 0    = (1, "movsb")
    | otherwise = (1, "movsw")

-- cmp
disasmB _ (1,0,1,0,0,1,1,w) xs
    | w == 0    = (1, "cmpsb")
    | otherwise = (1, "cmpsw")

-- scas
disasmB _ (1,0,1,0,1,1,1,w) xs
    | w == 0    = (1, "scasb")
    | otherwise = (1, "scasw")

-- lods
disasmB _ (1,0,1,0,1,1,0,w) xs
    | w == 0    = (1, "lodsb")
    | otherwise = (1, "lodsw")

-- stos
disasmB _ (1,0,1,0,1,0,1,w) xs
    | w == 0    = (1, "stosb")
    | otherwise = (1, "stosw")

-- call
-- Direct within Segment
disasmB ip (1,1,1,0,1,0,0,0) xs =
    (len, "call word " ++ imm)
    where
        len = 3
        imm = "0x" ++ hex (fromLE 2 xs + ip + len)

-- Indirect within Segment
disasmB _ (1,1,1,1,1,1,1,1) xs
    | r == 2 = (1 + len, "call " ++ rm)
    where
        (len, rm, r) = modrm True 1 xs

-- Direct Intersegment
disasmB ip (1,0,0,1,1,0,1,0) xs =
    (5, "call word " ++ immseg ++ ":" ++ immoff)
    where
        immseg = "0x" ++ hex (fromLE 2 (drop 2 xs))
        immoff = "0x" ++ hex (fromLE 2 (take 2 xs))

-- Indirect Intersegment
-- mod=11は実行不可能
disasmB _ (1,1,1,1,1,1,1,1) xs
    | r == 3 = (1 + len, "call word far " ++ rm)
    where
        (len, rm, r) = modrm False 1 xs

-- jmp
-- Direct within Segment
disasmB ip (1,1,1,0,1,0,0,1) xs =
    (len, "jmp word " ++ imm)
    where
        len = 3
        imm = "0x" ++ hex (fromLE 2 xs + ip + len)

-- Direct within Segment-Short
disasmB ip (1,1,1,0,1,0,1,1) xs =
    (len, "jmp short " ++ imm)
    where
        len = 2
        imm = "0x" ++ hex (fromLE 1 xs + ip + len)

-- Indirect within Segment
disasmB _ (1,1,1,1,1,1,1,1) xs
    | r == 4 = (1 + len, "jmp " ++ rm)
    where
        (len, rm, r) = modrm True 1 xs

-- Direct Intersegment
disasmB ip (1,1,1,0,1,0,1,0) xs =
    (5, "jmp word " ++ immseg ++ ":" ++ immoff)
    where
        immseg = "0x" ++ hex (fromLE 2 (drop 2 xs))
        immoff = "0x" ++ hex (fromLE 2 (take 2 xs))

-- Indirect Intersegment
-- mod=11は実行不可能
disasmB _ (1,1,1,1,1,1,1,1) xs
    | r == 5 = (1 + len, "jmp word far " ++ rm)
    where
        (len, rm, r) = modrm False 1 xs

-- ret
-- Within Segment
disasmB _ (1,1,0,0,0,0,1,1) xs =
    (1, "ret")

-- Within Seg Adding Immed to SP
disasmB _ (1,1,0,0,0,0,1,0) xs =
    (3, "ret " ++ imm)
    where
        imm = "0x" ++ hex (fromLE 2 xs)

-- Intersegment
disasmB _ (1,1,0,0,1,0,1,1) xs =
    (1, "retf")

-- Intersegment Adding Immediate to SP
disasmB _ (1,1,0,0,1,0,1,0) xs =
    (3, "retf " ++ imm)
    where
        imm = "0x" ++ hex (fromLE 2 xs)

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

