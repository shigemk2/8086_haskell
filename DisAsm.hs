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

disasm ip (x:xs) = disasmB ip "" (getBits x) xs

-- mov
-- Immediate to Register/Memory [100010dw][modregr/m]
disasmB _ seg (1,0,0,0,1,0,d,w) xs
    | d == 0    = (1 + len, "mov " ++ rm  ++ "," ++ reg)
    | otherwise = (1 + len, "mov " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm seg False w xs
        reg = regs !! w !! r

-- Immediate to Register [1011wreg][data][data if w=1]
disasmB _ _ (1,0,1,1,w,r,e,g) xs =
    (2 + w, "mov " ++ reg ++ "," ++ imm)
    where
        reg = regs !! w !! getReg r e g
        imm = "0x" ++ hex (fromLE (w + 1) xs)

-- Immediate to Register/Memory [1100011w][mod000r/m][data][data if w=1]
disasmB _ seg (1,1,0,0,0,1,1,w) xs =
    (1 + len + w + 1, "mov " ++ rm ++ "," ++ imm)
    where
        (len, rm, r) = modrm seg True w xs
        imm = "0x" ++ hex (fromLE (w + 1) (drop len xs))

-- Memory to Accumulator [1010000w][addr-low][addr-high]
disasmB _ seg (1,0,1,0,0,0,0,w) xs
    | w == 0    = (3, "mov " ++ rm ++ ",[" ++ seg ++ imm ++ "]")
    | otherwise = (3, "mov " ++ rm ++ ",[" ++ seg ++ imm ++ "]")
    where
        rm  = regs !! w !! 0
        imm = "0x" ++ hex (fromLE 2 xs)

-- Accumulator to Memory [1010001w][addr-low][addr-high]
disasmB _ seg (1,0,1,0,0,0,1,w) xs
    | w == 0    = (3, "mov [" ++ seg ++ imm ++ "]," ++ rm)
    | otherwise = (3, "mov [" ++ seg ++ imm ++ "]," ++ rm)
    where
        rm  = regs !! w !! 0
        imm = "0x" ++ hex (fromLE 2 xs)

-- Register/Memory to Segment Register [10001110][mod0reg r/m]
disasmB _ seg (1,0,0,0,1,1,1,0) xs =
    (1 + len, "mov " ++ rmseg ++ "," ++ rm)
    where
        (len, rm, r) = modrm seg False 1 xs
        rmseg = sreg !! r

-- Segment Register to Register/Memory [10001100][mod0reg r/m]
disasmB _ seg (1,0,0,0,1,1,0,0) xs =
    (1 + len, "mov " ++ rm ++ "," ++ rmseg)
    where
        (len, rm, r) = modrm seg False 1 xs
        rmseg = sreg !! r

-- push
-- Register/Memory
disasmB _ seg (1,1,1,1,1,1,1,1) xs
    | r == 6 = (1 + len, "push " ++ rm)
    where
        (len, rm, r) = modrm seg True 1 xs

-- Register
disasmB _ _ (0,1,0,1,0,r,e,g) xs =
    (1, "push " ++ reg)
    where
        reg = regs !! 1 !! getReg r e g

-- Segment Register
disasmB _ _ (0,0,0,s,r,1,1,0) xs =
    (1, "push " ++ rmseg)
    where
        rmseg = sreg !! getReg 0 s r

-- byte
disasmB _ _ (0,1,1,0,1,0,1,0) xs =
    (2, "push byte " ++ imm)
    where
        imm = disp8 (fromLE 1 xs)

-- pop
-- Register/Memory
disasmB _ seg (1,0,0,0,1,1,1,1) xs =
    (1 + len, "pop " ++ rm)
    where
        (len, rm, r) = modrm seg True 1 xs

-- Register
disasmB _ _ (0,1,0,1,1,r,e,g) xs =
    (1, "pop " ++ reg)
    where
        reg = regs !! 1 !! getReg r e g

-- Segment Register
disasmB _ _ (0,0,0,s,r,1,1,1) xs =
    (1, "pop " ++ rmseg)
    where
        rmseg = sreg !! getReg 0 s r

-- xchg
-- Register/Memory with Register
disasmB _ seg (1,0,0,0,0,1,1,w) xs =
    (1 + len, "xchg " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm seg False w xs
        reg = regs !! w !! r

-- Register with Accumulator
disasmB _ _ (1,0,0,1,0,r,e,g) xs
    -- xchg ax,axはなにもしていないのでnop
    | reg == "ax"    = (1, "nop")
    | otherwise      = (1, "xchg ax," ++ reg)
    where
        reg = regs !! 1 !! getReg r e g

-- in
-- Fixed Port
disasmB _ _ (1,1,1,0,0,1,0,w) (x:_)
    | w == 0    = (2, "in al," ++ imm)
    | otherwise = (2, "in ax," ++ imm)
    where
        imm = "0x" ++ hex x

-- in
-- Variable Port
disasmB _ _ (1,1,1,0,1,1,0,w) xs
    | w == 0    = (1, "in al,dx")
    | otherwise = (1, "in ax,dx")

-- out
-- Fixed Port
disasmB _ _ (1,1,1,0,0,1,1,w) (x:_)
    | w == 0    = (2, "out " ++ imm ++ ",al")
    | otherwise = (2, "out " ++ imm ++ ",ax")
    where
        imm = "0x" ++ hex x

-- out
-- Variable Port
disasmB _ _ (1,1,1,0,1,1,1,w) xs
    | w == 0    = (1, "out dx,al")
    | otherwise = (1, "out dx,ax")

-- xlat
disasmB _ _ (1,1,0,1,0,1,1,1) xs = mne1 "xlatb"

-- lea
disasmB _ seg (1,0,0,0,1,1,0,1) xs =
    (1 + len, "lea " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm seg False 1 xs
        reg = regs !! 1 !! r

-- lds
disasmB _ seg (1,1,0,0,0,1,0,1) xs =
    (1 + len, "lds " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm seg False 1 xs
        reg = regs !! 1 !! r

-- les
disasmB _ seg (1,1,0,0,0,1,0,0) xs =
    (1 + len, "les " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm seg False 1 xs
        reg = regs !! 1 !! r

-- lahf
disasmB _ _ (1,0,0,1,1,1,1,1) xs = mne1 "lahf"

-- sahf
disasmB _ _ (1,0,0,1,1,1,1,0) xs = mne1 "sahf"

-- pushf
disasmB _ _ (1,0,0,1,1,1,0,0) xs = mne1 "pushfw"

-- popf
disasmB _ _ (1,0,0,1,1,1,0,1) xs = mne1 "popfw"

disasmB _ _ (0,0,0,0,0,0,0,0) [] =
    (1, "db 0x00")

-- add
-- Reg./Memory with Register to Either
disasmB _ seg (0,0,0,0,0,0,d,w) xs
    | d == 0    = (1 + len, "add " ++ rm  ++ "," ++ reg)
    | otherwise = (1 + len, "add " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm seg False w xs
        reg = regs !! w !! r

-- Immediate to Register/Memory
disasmB _ seg (1,0,0,0,0,0,s,w) xs
    -- s w = 10 のときは欠番
    | getReg 0 s w == 2           = (1, "db 0x82")
    | getReg 0 s w == 3 && r == 0 = (1 + len + 1, "add " ++ rm ++ ",byte " ++ imms)
    |                      r == 0 = (1 + len + w + 1, "add " ++ rm ++ "," ++ imm)
    where
        (len, rm, r) = modrm seg True w xs
        imms = disp8 (fromLE 1 (drop len xs))
        imm  = "0x" ++ hex (fromLE (w + 1) (drop len xs))

-- Immediate to Accumulator
disasmB _ _ (0,0,0,0,0,1,0,w) xs
    | w == 0    = (2, "add al," ++ imm)
    | otherwise = (3, "add ax," ++ imm)
    where
        imm = "0x" ++ hex (fromLE (1 + w) xs)

-- adc
-- Reg./Memory with Register to Either
disasmB _ seg (0,0,0,1,0,0,d,w) xs
    | d == 0    = (1 + len, "adc " ++ rm  ++ "," ++ reg)
    | otherwise = (1 + len, "adc " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm seg False w xs
        reg = regs !! w !! r

-- Immediate to Register/Memory
disasmB _ seg (1,0,0,0,0,0,s,w) xs
    -- s w = 10 のときは欠番
    | getReg 0 s w == 2           = (1, "db 0x82")
    | getReg 0 s w == 3 && r == 2 = (1 + len + 1, "adc " ++ rm ++ ",byte +" ++ imms)
    |                      r == 2 = (1 + len + w + 1, "adc " ++ rm ++ "," ++ imm)
    where
        (len, rm, r) = modrm seg True w xs
        imms = "0x" ++ hex (fromLE 1 (drop len xs))
        imm  = "0x" ++ hex (fromLE (w + 1) (drop len xs))

-- Immediate to Accumulator
disasmB _ _ (0,0,0,1,0,1,0,w) xs
    | w == 0    = (2, "adc al," ++ imm)
    | otherwise = (3, "adc ax," ++ imm)
    where
        imm = "0x" ++ hex (fromLE (1 + w) xs)

-- inc
-- Register/Memory
disasmB _ seg (1,1,1,1,1,1,1,w) xs
    | w == 0 && r == 0 = (1 + len, "inc " ++ rm)
    | w == 1 && r == 0 = (1 + len, "inc " ++ rm)
    where
        (len, rm, r) = modrm seg True w xs

-- Register
disasmB _ _ (0,1,0,0,0,r,e,g) xs =
    (1, "inc " ++ reg)
    where
        reg = regs !! 1 !! getReg r e g

-- aaa
disasmB _ _ (0,0,1,1,0,1,1,1) xs = mne1 "aaa"

-- daa
disasmB _ _ (0,0,1,0,0,1,1,1) xs = mne1 "daa"

-- sub
-- Reg./Memory and Register to Either
disasmB _ seg (0,0,1,0,1,0,d,w) xs
    | d == 0    = (1 + len, "sub " ++ rm  ++ "," ++ reg)
    | otherwise = (1 + len, "sub " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm seg False w xs
        reg = regs !! w !! r

-- Immediate to Register/Memory
disasmB _ seg (1,0,0,0,0,0,s,w) xs
    -- s w = 10 のときは欠番
    | getReg 0 s w == 2           = (1, "db 0x82")
    | getReg 0 s w == 3 && r == 5 = (1 + len + 1, "sub " ++ rm ++ ",byte +" ++ imms)
    |                      r == 5 = (1 + len + w + 1, "sub " ++ rm ++ "," ++ imm)
    where
        (len, rm, r) = modrm seg True w xs
        imms = "0x" ++ hex (fromLE 1 (drop len xs))
        imm  = "0x" ++ hex (fromLE (w + 1) (drop len xs))

-- Immediate from Accumulator
disasmB _ _ (0,0,1,0,1,1,0,w) xs
    | w == 0    = (2, "sub al," ++ imm)
    | otherwise = (3, "sub ax," ++ imm)
    where
        imm = "0x" ++ hex (fromLE (1 + w) xs)

-- sbb
-- Reg./Memory and Register to Either
disasmB _ seg (0,0,0,1,1,0,d,w) xs
    | d == 0    = (1 + len, "sbb " ++ rm  ++ "," ++ reg)
    | otherwise = (1 + len, "sbb " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm seg False w xs
        reg = regs !! w !! r

-- Immediate to Register/Memory
disasmB _ seg (1,0,0,0,0,0,s,w) xs
    -- s w = 10 のときは欠番
    | getReg 0 s w == 2           = (1, "db 0x82")
    | getReg 0 s w == 3 && r == 3 = (1 + len + 1, "sbb " ++ rm ++ ",byte +" ++ imms)
    |                      r == 3 = (1 + len + w + 1, "sbb " ++ rm ++ "," ++ imm)
    where
        (len, rm, r) = modrm seg True w xs
        imms = "0x" ++ hex (fromLE 1 (drop len xs))
        imm  = "0x" ++ hex (fromLE (w + 1) (drop len xs))

-- Immediate to Accumulator
disasmB _ _ (0,0,0,1,1,1,0,w) xs
    | w == 0    = (2, "sbb al," ++ imm)
    | otherwise = (3, "sbb ax," ++ imm)
    where
        imm = "0x" ++ hex (fromLE (1 + w) xs)

-- dec
-- Register/Memory
disasmB _ seg (1,1,1,1,1,1,1,w) xs
    | w == 0 && r == 1 = (1 + len, "dec " ++ rm)
    | w == 1 && r == 1 = (1 + len, "dec " ++ rm)
    where
        (len, rm, r) = modrm seg True w xs

-- Register
disasmB _ _ (0,1,0,0,1,r,e,g) xs =
    (1, "dec " ++ reg)
    where
        reg = regs !! 1 !! getReg r e g

-- neg
disasmB _ seg (1,1,1,1,0,1,1,w) xs
    | r == 3 = (1 + len, "neg " ++ rm)
    where
        (len, rm, r) = modrm seg True w xs

-- cmp
-- Register/Memory and Register
disasmB _ seg (0,0,1,1,1,0,d,w) xs
    | d == 0    = (1 + len, "cmp " ++ rm  ++ "," ++ reg)
    | otherwise = (1 + len, "cmp " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm seg False w xs
        reg = regs !! w !! r

-- Immediate to Register/Memory
disasmB _ seg (1,0,0,0,0,0,s,w) xs
    -- s w = 10 のときは欠番
    | getReg 0 s w == 2           = (1, "db 0x82")
    | getReg 0 s w == 3 && r == 7 = (1 + len + 1, "cmp " ++ rm ++ ",byte " ++ imms)
    |                      r == 7 = (1 + len + w + 1, "cmp " ++ rm ++ "," ++ imm)
    where
        (len, rm, r) = modrm seg True w xs
        imms = disp8 (fromLE 1 (drop len xs))
        imm  = "0x" ++ hex (fromLE (w + 1) (drop len xs))

-- Immediate with Accumulator
disasmB _ _ (0,0,1,1,1,1,0,w) xs
    | w == 0    = (2, "cmp al," ++ imm)
    | otherwise = (3, "cmp ax," ++ imm)
    where
        imm = "0x" ++ hex (fromLE (1 + w) xs)

-- aas
disasmB _ _ (0,0,1,1,1,1,1,1) xs = mne1 "aas"

-- das
disasmB _ _ (0,0,1,0,1,1,1,1) xs = mne1 "das"

-- mul
disasmB _ seg (1,1,1,1,0,1,1,w) xs
    | r == 4 = (1 + len, "mul " ++ rm)
    where
        (len, rm, r) = modrm seg True w xs

-- imul
disasmB _ seg (1,1,1,1,0,1,1,w) xs
    | r == 5 = (1 + len, "imul " ++ rm)
    where
        (len, rm, r) = modrm seg True w xs

-- aam
disasmB _ _ (1,1,0,1,0,1,0,0) xs
    | getBits (head xs) == (0,0,0,0,1,0,1,0) = (2, "aam")
    -- | xs == [0x0a] = (2, "aam")

-- div
disasmB _ seg (1,1,1,1,0,1,1,w) xs
    | r == 6 = (1 + len, "div " ++ rm)
    where
        (len, rm, r) = modrm seg True w xs

-- idiv
disasmB _ seg (1,1,1,1,0,1,1,w) xs
    | r == 7 = (1 + len, "idiv " ++ rm)
    where
        (len, rm, r) = modrm seg True w xs

-- aad
disasmB _ _ (1,1,0,1,0,1,0,1) xs
    | getBits (head xs) == (0,0,0,0,1,0,1,0) = (2, "aad")

-- cbw
disasmB _ _ (1,0,0,1,1,0,0,0) xs = mne1 "cbw"

-- cwd
disasmB _ _ (1,0,0,1,1,0,0,1) xs = mne1 "cwd"

-- not
disasmB _ seg (1,1,1,1,0,1,1,w) xs
    | r == 2 = (1 + len, "not " ++ rm)
    where
        (len, rm, r) = modrm seg True w xs

-- shl/sal
disasmB _ seg (1,1,0,1,0,0,v,w) xs
    | r == 4 && v == 0 = (len + 1, "shl " ++ rm ++ ",1")
    | r == 4 && v == 1 = (len + 1, "shl " ++ rm ++ ",cl")
    where
        (len, rm, r) = modrm seg True w xs

-- shr
disasmB _ seg (1,1,0,1,0,0,v,w) xs
    | r == 5 && v == 0 = (len + 1, "shr " ++ rm ++ ",1")
    | r == 5 && v == 1 = (len + 1, "shr " ++ rm ++ ",cl")
    where
        (len, rm, r) = modrm seg True w xs

-- sar
disasmB _ seg (1,1,0,1,0,0,v,w) xs
    | r == 7 && v == 0 = (len + 1, "sar " ++ rm ++ ",1")
    | r == 7 && v == 1 = (len + 1, "sar " ++ rm ++ ",cl")
    where
        (len, rm, r) = modrm seg True w xs

-- rol
disasmB _ seg (1,1,0,1,0,0,v,w) xs
    | r == 0 && v == 0 = (len + 1, "rol " ++ rm ++ ",1")
    | r == 0 && v == 1 = (len + 1, "rol " ++ rm ++ ",cl")
    where
        (len, rm, r) = modrm seg True w xs

-- ror
disasmB _ seg (1,1,0,1,0,0,v,w) xs
    | r == 1 && v == 0 = (len + 1, "ror " ++ rm ++ ",1")
    | r == 1 && v == 1 = (len + 1, "ror " ++ rm ++ ",cl")
    where
        (len, rm, r) = modrm seg True w xs

-- rcl
disasmB _ seg (1,1,0,1,0,0,v,w) xs
    | r == 2 && v == 0 = (len + 1, "rcl " ++ rm ++ ",1")
    | r == 2 && v == 1 = (len + 1, "rcl " ++ rm ++ ",cl")
    where
        (len, rm, r) = modrm seg True w xs

-- rcr
disasmB _ seg (1,1,0,1,0,0,v,w) xs
    | r == 3 && v == 0 = (len + 1, "rcr " ++ rm ++ ",1")
    | r == 3 && v == 1 = (len + 1, "rcr " ++ rm ++ ",cl")
    where
        (len, rm, r) = modrm seg True w xs

-- and
-- Reg./Memory and Register to Either
disasmB _ seg (0,0,1,0,0,0,d,w) xs
    | d == 0    = (1 + len, "and " ++ rm  ++ "," ++ reg)
    | otherwise = (1 + len, "and " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm seg False w xs
        reg = regs !! w !! r

-- Immediate to Register/Memory
disasmB _ seg (1,0,0,0,0,0,0,w) xs
    | w == 0 && r == 4 = (2 + len, "and " ++ rm ++ "," ++ imm)
    | w == 1 && r == 4 = (2 + 1 + len, "and " ++ rm ++ "," ++ imm)
    where
        (len, rm, r) = modrm seg True w xs
        imm = "0x" ++ hex (fromLE (w + 1) (drop len xs))

-- Immediate to Accumulator
disasmB _ _ (0,0,1,0,0,1,0,w) xs
    | w == 0    = (2, "and al," ++ imm)
    | otherwise = (3, "and ax," ++ imm)
    where
        imm = "0x" ++ hex (fromLE (1 + w) xs)

-- test
-- Register/Memory and Register
disasmB _ seg (1,0,0,0,0,1,0,w) xs
    | w == 0    = (1 + len, "test " ++ rm ++ "," ++ reg)
    | otherwise = (1 + len, "test " ++ rm ++ "," ++ reg)
    where
        (len, rm, r) = modrm seg False w xs
        reg = regs !! w !! r

-- Immediate Data and Register/Memory
disasmB _ seg (1,1,1,1,0,1,1,w) xs
    | r == 0 = (2 + len + w, "test " ++ rm ++ "," ++ imm)
    where
        (len, rm, r) = modrm seg True w xs
        imm = "0x" ++ hex (fromLE (1 + w) (drop len xs))

-- Immediate Data and Accumulator
disasmB _ _ (1,0,1,0,1,0,0,w) xs
    | w == 0    = (2 + w, "test " ++ "al," ++ imm)
    | otherwise = (2 + w, "test " ++ "ax," ++ imm)
    where
        imm = "0x" ++ hex (fromLE (1 + w) xs)

-- or
-- Reg./Memory and Register to Either
disasmB _ seg (0,0,0,0,1,0,d,w) xs
    | d == 0    = (1 + len, "or " ++ rm  ++ "," ++ reg)
    | otherwise = (1 + len, "or " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm seg False w xs
        reg = regs !! w !! r

-- Immediate to Register/Memory
disasmB _ seg (1,0,0,0,0,0,0,w) xs
    | w == 0 && r == 1 = (2 + len, "or " ++ rm ++ "," ++ imm)
    | w == 1 && r == 1 = (2 + 1 + len, "or " ++ rm ++ "," ++ imm)
    where
        (len, rm, r) = modrm seg True w xs
        imm = "0x" ++ hex (fromLE (w + 1) (drop len xs))

-- Immediate to Accumulator
disasmB _ _ (0,0,0,0,1,1,0,w) xs
    | w == 0    = (2, "or al," ++ imm)
    | otherwise = (3, "or ax," ++ imm)
    where
        imm = "0x" ++ hex (fromLE (1 + w) xs)

-- xor
-- Reg./Memory and Register to Either
disasmB _ seg (0,0,1,1,0,0,d,w) xs
    | d == 0    = (1 + len, "xor " ++ rm  ++ "," ++ reg)
    | otherwise = (1 + len, "xor " ++ reg ++ "," ++ rm)
    where
        (len, rm, r) = modrm seg False w xs
        reg = regs !! w !! r

-- Immediate to Register/Memory
disasmB _ seg (1,0,0,0,0,0,0,w) xs
    | w == 0 && r == 6 = (2 + len, "xor " ++ rm ++ "," ++ imm)
    | w == 1 && r == 6 = (2 + 1 + len, "xor " ++ rm ++ "," ++ imm)
    where
        (len, rm, r) = modrm seg True w xs
        imm = "0x" ++ hex (fromLE (w + 1) (drop len xs))

-- Immediate to Accumulator
disasmB _ _ (0,0,1,1,0,1,0,w) xs
    | w == 0    = (2, "xor al," ++ imm)
    | otherwise = (3, "xor ax," ++ imm)
    where
        imm = "0x" ++ hex (fromLE (1 + w) xs)

-- rep
disasmB ip seg (1,1,1,1,0,0,1,z) (x:xs)
    | z == 1 && hex x == "a6" = (1 + len, "repe " ++ disasm')
    | z == 1 && hex x == "a7" = (1 + len, "repe " ++ disasm')
    | z == 1 && hex x == "ae" = (1 + len, "repe " ++ disasm')
    | z == 1 && hex x == "af" = (1 + len, "repe " ++ disasm')
    | z == 0                  = (1 + len, "repne " ++ disasm')
    | otherwise               = (1 + len, "rep " ++ disasm')
    where
        len    = fst $ disasm ip (x:xs)
        disasm' = snd $ disasm ip (x:xs)

-- movs
disasmB _ _ (1,0,1,0,0,1,0,w) xs
    | w == 0    = mne1 "movsb"
    | otherwise = mne1 "movsw"

-- cmp
disasmB _ _ (1,0,1,0,0,1,1,w) xs
    | w == 0    = mne1 "cmpsb"
    | otherwise = mne1 "cmpsw"

-- scas
disasmB _ _ (1,0,1,0,1,1,1,w) xs
    | w == 0    = mne1 "scasb"
    | otherwise = mne1 "scasw"

-- lods
disasmB _ _ (1,0,1,0,1,1,0,w) xs
    | w == 0    = mne1 "lodsb"
    | otherwise = mne1 "lodsw"

-- stos
disasmB _ _ (1,0,1,0,1,0,1,w) xs
    | w == 0    = mne1 "stosb"
    | otherwise = mne1 "stosw"

-- call
-- Direct within Segment
disasmB ip seg (1,1,1,0,1,0,0,0) xs =
    (len, "call word " ++ imm)
    where
        len = 3
        imm = "0x" ++ hex ((fromLE 2 xs + ip + len) .&. 0xffff)

-- Indirect within Segment
disasmB _ seg (1,1,1,1,1,1,1,1) xs
    | r == 2 = (1 + len, "call " ++ rm)
    where
        (len, rm, r) = modrm seg True 1 xs

-- Direct Intersegment
disasmB ip seg (1,0,0,1,1,0,1,0) xs =
    (5, "call word " ++ immseg ++ ":" ++ immoff)
    where
        immseg = "0x" ++ hex (fromLE 2 (drop 2 xs))
        immoff = "0x" ++ hex (fromLE 2 (take 2 xs))

-- Indirect Intersegment
-- mod=11は実行不可能
disasmB _ seg (1,1,1,1,1,1,1,1) xs
    | r == 3 = (1 + len, "call word far " ++ rm)
    where
        (len, rm, r) = modrm seg False 1 xs

-- jmp
-- Direct within Segment
disasmB ip seg (1,1,1,0,1,0,0,1) xs =
    (len, "jmp word " ++ imm)
    where
        len = 3
        imm = "0x" ++ hex ((fromLE 2 xs + ip + len) .&. 0xffff)

-- Direct within Segment-Short
disasmB ip seg (1,1,1,0,1,0,1,1) xs = rel8 "jmp short" ip xs

-- Indirect within Segment
disasmB _ seg (1,1,1,1,1,1,1,1) xs
    | r == 4 = (1 + len, "jmp " ++ rm)
    where
        (len, rm, r) = modrm seg True 1 xs

-- Direct Intersegment
disasmB ip seg (1,1,1,0,1,0,1,0) xs =
    (5, "jmp word " ++ immseg ++ ":" ++ immoff)
    where
        immseg = "0x" ++ hex (fromLE 2 (drop 2 xs))
        immoff = "0x" ++ hex (fromLE 2 (take 2 xs))

-- Indirect Intersegment
-- mod=11は実行不可能
disasmB _ seg (1,1,1,1,1,1,1,1) xs
    | r == 5 = (1 + len, "jmp word far " ++ rm)
    where
        (len, rm, r) = modrm seg False 1 xs

-- ret
-- Within Segment
disasmB _ _ (1,1,0,0,0,0,1,1) xs = mne1 "ret"

-- Within Seg Adding Immed to SP
disasmB _ _ (1,1,0,0,0,0,1,0) xs =
    (3, "ret " ++ imm)
    where
        imm = "0x" ++ hex (fromLE 2 xs)

-- Intersegment
disasmB _ _ (1,1,0,0,1,0,1,1) xs = mne1 "retf"

-- Intersegment Adding Immediate to SP
disasmB _ _ (1,1,0,0,1,0,1,0) xs =
    (3, "retf " ++ imm)
    where
        imm = "0x" ++ hex (fromLE 2 xs)

-- je/jz
disasmB ip seg (0,1,1,1,0,1,0,0) xs = rel8 "jz" ip xs

-- jl/jnge
disasmB ip seg (0,1,1,1,1,1,0,0) xs = rel8 "jl" ip xs

-- jle/jng
disasmB ip seg (0,1,1,1,1,1,1,0) xs = rel8 "jng" ip xs

-- jb/jnae(jcはエイリアス)
disasmB ip seg (0,1,1,1,0,0,1,0) xs = rel8 "jc" ip xs

-- jbe/jna
disasmB ip seg (0,1,1,1,0,1,1,0) xs = rel8 "jna" ip xs

-- jp/jpe
disasmB ip seg (0,1,1,1,1,0,1,0) xs = rel8 "jpe" ip xs

-- jo
disasmB ip seg (0,1,1,1,0,0,0,0) xs = rel8 "jo" ip xs

-- js
disasmB ip seg (0,1,1,1,1,0,0,0) xs = rel8 "js" ip xs

-- jne/jnz
disasmB ip seg (0,1,1,1,0,1,0,1) xs = rel8 "jnz" ip xs

-- jnl/jge
disasmB ip seg (0,1,1,1,1,1,0,1) xs = rel8 "jnl" ip xs

-- jnle/jg
disasmB ip seg (0,1,1,1,1,1,1,1) xs = rel8 "jg" ip xs

-- jnb/jae
disasmB ip seg (0,1,1,1,0,0,1,1) xs = rel8 "jnc" ip xs

-- jnbe/ja
disasmB ip seg (0,1,1,1,0,1,1,1) xs = rel8 "ja" ip xs

-- jnp/jpo
disasmB ip seg (0,1,1,1,1,0,1,1) xs = rel8 "jpo" ip xs

-- jno
disasmB ip seg (0,1,1,1,0,0,0,1) xs = rel8 "jno" ip xs

-- jns
disasmB ip seg (0,1,1,1,1,0,0,1) xs = rel8 "jns" ip xs

-- loop
disasmB ip seg (1,1,1,0,0,0,1,0) xs = rel8 "loop" ip xs

-- loopz/loope
disasmB ip seg (1,1,1,0,0,0,0,1) xs = rel8 "loope" ip xs

-- loopnz/loopne
disasmB ip seg (1,1,1,0,0,0,0,0) xs = rel8 "loopne" ip xs

-- jcxz
disasmB ip seg (1,1,1,0,0,0,1,1) xs = rel8 "jcxz" ip xs

-- int
-- Type Specified
disasmB ip seg (1,1,0,0,1,1,0,1) (x:_) =
    (2, "int " ++ imm)
    where
        imm = "0x" ++ hex x

-- Type 3
disasmB _ _ (1,1,0,0,1,1,0,0) xs = mne1 "int3"

-- into
disasmB _ _ (1,1,0,0,1,1,1,0) xs = mne1 "into"

-- iretw
disasmB _ _ (1,1,0,0,1,1,1,1) xs = mne1 "iretw"

-- clc
disasmB _ _ (1,1,1,1,1,0,0,0) xs = mne1 "clc"

-- cmc
disasmB _ _ (1,1,1,1,0,1,0,1) xs = mne1 "cmc"

-- stc
disasmB _ _ (1,1,1,1,1,0,0,1) xs = mne1 "stc"

-- cld
disasmB _ _ (1,1,1,1,1,1,0,0) xs = mne1 "cld"

-- std
disasmB _ _ (1,1,1,1,1,1,0,1) xs = mne1 "std"

-- cli
disasmB _ _ (1,1,1,1,1,0,1,0) xs = mne1 "cli"

-- sti
disasmB _ _ (1,1,1,1,1,0,1,1) xs = mne1 "sti"

-- hlt
disasmB _ _ (1,1,1,1,0,1,0,0) xs = mne1 "hlt"

-- wait
disasmB _ _ (1,0,0,1,1,0,1,1) xs = mne1 "wait"

-- lock
disasmB _ _ (1,1,1,1,0,0,0,0) xs = mne1 "lock"

-- segment override prefix
disasmB ip seg (0,0,1,s,r,1,1,0) (x:xs) =
    (fst asm + 1, snd asm)
    where
       seg = sreg !! getReg 0 s r ++ ":"
       asm = disasmB ip seg (getBits x) xs

-- pushaw
disasmB _ _ (0,1,1,0,0,0,0,0) xs = mne1 "pushaw"

-- push word
disasmB ip seg (0,1,1,0,1,0,0,0) xs =
    (3, "push word " ++ imm)
    where
        imm = "0x" ++ hex (fromLE 2 xs)

-- popaw
disasmB _ _ (0,1,1,0,0,0,0,1) xs = mne1 "popaw"

disasmB _ _ x xs =
    (1, "db 0x" ++ hex (getByte x))

regad = ["bx+si", "bx+di", "bp+si", "bp+di", "si", "di", "bp", "bx"]

modrm seg prefix w (x:xs) = (len, s, reg)
    where
        (len, s) = f mode rm
        mode =  x `shiftR` 6
        reg  = (x `shiftR` 3) .&. 7
        rm   =  x             .&. 7
        pfx | prefix && w == 0 = "byte "
            | prefix && w == 1 = "word "
            | otherwise        = ""
        f 0 6  = (3, pfx ++ "[" ++ seg ++ "0x" ++ hex (fromLE 2 xs) ++ "]")
        f 0 rm = (1, pfx ++ "[" ++ seg ++ regad !! rm ++ "]")
        f 1 rm = (2, pfx ++ "[" ++ seg ++ regad !! rm ++ disp ++ "]")
            where
                disp = disp8 (xs !! 0)
        f 2 rm = (3, pfx ++ "[" ++ seg ++ regad !! rm ++ disp ++ "]")
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

getByte :: (Int,Int,Int,Int,Int,Int,Int,Int) -> Int
getByte (a,b,c,d,e,f,g,h) = y
    where
        a' = a `shiftL` 7
        b' = b `shiftL` 6
        c' = c `shiftL` 5
        d' = d `shiftL` 4
        e' = e `shiftL` 3
        f' = f `shiftL` 2
        g' = g `shiftL` 1
        y  = a'.|.b'.|.c'.|.d'.|.e'.|.f'.|.g'.|.h

getReg :: Int -> Int -> Int -> Int
getReg r e g =
    (r `shiftL` 2) .|. (e `shiftL` 1) .|. g

disp8 x
    | x < 0x80  = "+0x" ++ hex x
    | otherwise = "-0x" ++ hex (0x100 - x)

disp16 x
    | x < 0x8000  = "+0x" ++ hex x
    | otherwise = "-0x" ++ hex (0x10000 - x)

rel8 mne ip (x:_) = (2, mne ++ " 0x" ++ hex (f .&. 0xffff))
    where
        f | x < 0x80  = ip + 2 + x
          | otherwise = ip + 2 - (0x100 - x)

mne1 mne = (1, mne)
