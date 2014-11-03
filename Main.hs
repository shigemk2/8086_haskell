module Main where

import Test.HUnit
import System.IO
import Data.Char
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

testDisAsm = TestList
    [ "b8 1" ~: disasm [0xb8, 0, 0]       ~?= "mov ax,0x0"
    , "b8 2" ~: disasm [0xb8, 0x34, 0x12] ~?= "mov ax,0x1234"
    , "b8 2" ~: disasm [0xb8, 0x78, 0x56] ~?= "mov ax,0x5678"
    , "b8 3" ~: disasm' "b80000" ~?= "mov ax,0x0"
    , "b8 4" ~: disasm' "b83412" ~?= "mov ax,0x1234"
    , "b8-bf 0" ~: disasm' "b80100" ~?= "mov ax,0x1"
    , "b8-bf 1" ~: disasm' "b90100" ~?= "mov cx,0x1"
    , "b8-bf 2" ~: disasm' "ba1000" ~?= "mov dx,0x10"
    , "b8-bf 3" ~: disasm' "bb0001" ~?= "mov bx,0x100"
    , "b8-bf 4" ~: disasm' "bc0010" ~?= "mov sp,0x1000"
    , "b8-bf 5" ~: disasm' "bdff00" ~?= "mov bp,0xff"
    , "b8-bf 6" ~: disasm' "be00ff" ~?= "mov si,0xff00"
    , "b8-bf 7" ~: disasm' "bffeca" ~?= "mov di,0xcafe"
    , "b0-b7 1" ~: disasm' "b000" ~?= "mov al,0x0"
    , "b0-b7 2" ~: disasm' "b101" ~?= "mov cl,0x1"
    , "b0-b7 3" ~: disasm' "b210" ~?= "mov dl,0x10"
    , "b0-b7 4" ~: disasm' "b311" ~?= "mov bl,0x11"
    , "b0-b7 5" ~: disasm' "b412" ~?= "mov ah,0x12"
    , "b0-b7 6" ~: disasm' "b5ff" ~?= "mov ch,0xff"
    , "b0-b7 7" ~: disasm' "b6ee" ~?= "mov dh,0xee"
    , "b0-b7 8" ~: disasm' "b7ca" ~?= "mov bh,0xca"
    , "getBits" ~: getBits 0xbd ~?= (1,0,1,1,1,1,0,1)
    , "getReg" ~: getReg 1 0 1 ~?= 5
    , "88-8b mod=00,r/m=000 1" ~: disasm' "8800" ~?= "mov [bx+si],al"
    , "88-8b mod=00,r/m=000 2" ~: disasm' "8900" ~?= "mov [bx+si],ax"
    , "88-8b mod=00,r/m=000 3" ~: disasm' "8A00" ~?= "mov al,[bx+si]"
    , "88-8b mod=00,r/m=000 4" ~: disasm' "8B00" ~?= "mov ax,[bx+si]"
    , "88-8b mod=00,r/m=110 1" ~: disasm' "88063412" ~?= "mov [0x1234],al"
    , "88-8b mod=00,r/m=110 2" ~: disasm' "89063412" ~?= "mov [0x1234],ax"
    , "88-8b mod=00,r/m=110 3" ~: disasm' "8A063412" ~?= "mov al,[0x1234]"
    , "88-8b mod=00,r/m=110 4" ~: disasm' "8B063412" ~?= "mov ax,[0x1234]"
    , "88-8b mod=00 1" ~: disasm' "8900" ~?= "mov [bx+si],ax"
    , "88-8b mod=00 2" ~: disasm' "8909" ~?= "mov [bx+di],cx"
    , "88-8b mod=00 3" ~: disasm' "8912" ~?= "mov [bp+si],dx"
    , "88-8b mod=00 4" ~: disasm' "891b" ~?= "mov [bp+di],bx"
    , "88-8b mod=00 5" ~: disasm' "8924" ~?= "mov [si],sp"
    , "88-8b mod=00 6" ~: disasm' "892d" ~?= "mov [di],bp"
    , "88-8b mod=00 7" ~: disasm' "893f" ~?= "mov [bx],di"
    , "disp8 1" ~: disp8 0    ~?= "+0x0"
    , "disp8 2" ~: disp8 0x7f ~?= "+0x7f"
    , "disp8 3" ~: disp8 0x80 ~?= "-0x80"
    , "disp8 4" ~: disp8 0xff ~?= "-0x1"
    , "88-8b mod=01 1" ~: disasm' "894001" ~?= "mov [bx+si+0x1],ax"
    , "88-8b mod=01 2" ~: disasm' "8949FF" ~?= "mov [bx+di-0x1],cx"
    , "88-8b mod=01 3" ~: disasm' "895202" ~?= "mov [bp+si+0x2],dx"
    , "88-8b mod=01 4" ~: disasm' "895BFE" ~?= "mov [bp+di-0x2],bx"
    , "88-8b mod=01 5" ~: disasm' "896464" ~?= "mov [si+0x64],sp"
    , "88-8b mod=01 6" ~: disasm' "896D9C" ~?= "mov [di-0x64],bp"
    , "88-8b mod=01 7" ~: disasm' "897600" ~?= "mov [bp+0x0],si"
    , "88-8b mod=01 8" ~: disasm' "897601" ~?= "mov [bp+0x1],si"
    , "88-8b mod=01 9" ~: disasm' "897F01" ~?= "mov [bx+0x1],di"
    ]

main = do
    runTestText (putTextToHandle stderr False) (TestList [testHex, testDisAsm])
