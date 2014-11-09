module Main where

import Test.HUnit
import System.IO
import Data.Char
import Data.Bits
import Hex
import DisAsm

testHex = TestList
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
                , "toLE 2" ~: toLE 2 0x10000    ~?= [0, 0]
        , "toLE 3" ~: toLE 4 0x12345678 ~?= [0x78, 0x56, 0x34, 0x12]
        , "fromLE 1" ~: fromLE 2 [0, 1]                   ~?= 0x100
        , "fromLE 2" ~: fromLE 2 [0x78, 0x56, 0x34, 0x12] ~?= 0x5678
        , "fromLE 3" ~: fromLE 4 [0x78, 0x56, 0x34, 0x12] ~?= 0x12345678
        , "fromLE 4" ~: fromLE 2 [0x00, 0xFF] ~?= 0xFF00
        , "toBE 1" ~: toBE 2 1          ~?= [0, 1]
        , "toBE 2" ~: toBE 2 0x10000    ~?= [0, 0]
        , "toBE 3" ~: toBE 4 0x12345678 ~?= [0x12, 0x34, 0x56, 0x78]
        , "fromBE 1" ~: fromBE 2 [0, 1]                   ~?= 0x1
        , "fromBE 2" ~: fromBE 2 [0x78, 0x56, 0x34, 0x12] ~?= 0x7856
        , "fromBE 3" ~: fromBE 4 [0x78, 0x56, 0x34, 0x12] ~?= 0x78563412
        ]

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
    , "getBits 0" ~: getBits 0xbd ~?= (1,0,1,1,1,1,0,1)
    , "getBits 1" ~: getBits 0xb8 ~?= (1,0,1,1,1,0,0,0)
    , "getBits 2" ~: getBits 0xb9 ~?= (1,0,1,1,1,0,0,1)
    , "getReg 0" ~: getReg 1 0 1 ~?= 5
    , "getReg 1" ~: getReg 1 1 1 ~?= 7
    , "getReg 2" ~: getReg 0 0 1 ~?= 1
    , "disAsmB 0" ~: disasmB (1,0,1,1,1,0,0,0) [0, 0] ~?= "mov ax,0x0"
    , "disAsmB 1" ~: disasmB (1,0,0,0,1,0,1,1) [0, 0] ~?= "mov ax,[bx+si]"
    , "disAsmB 2" ~: disasmB (1,0,0,0,1,0,0,1) [0, 0] ~?= "mov [bx+si],ax"
    , "disAsmB 3" ~: disasmB (1,0,0,0,1,0,0,0) [0, 0] ~?= "mov [bx+si],al"
    , "disAsmB 4" ~: disasmB (1,0,0,0,1,0,0,0) [2, 0] ~?= "mov [bp+si],al"
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
    , "88-8b mod=00,r/m=000 1" ~: disasm' "8800" ~?= "mov [bx+si],al"
    , "88-8b mod=00,r/m=000 2" ~: disasm' "8900" ~?= "mov [bx+si],ax"
    , "88-8b mod=00,r/m=000 3" ~: disasm' "8A00" ~?= "mov al,[bx+si]"
    , "88-8b mod=00,r/m=000 4" ~: disasm' "8B00" ~?= "mov ax,[bx+si]"
    , "88-8b mod=00,r/m=110 1" ~: disasm' "88063412" ~?= "mov [0x1234],al"
    , "88-8b mod=00,r/m=110 2" ~: disasm' "89063412" ~?= "mov [0x1234],ax"
    , "88-8b mod=00,r/m=110 3" ~: disasm' "8A063412" ~?= "mov al,[0x1234]"
    , "88-8b mod=00,r/m=110 4" ~: disasm' "8B063412" ~?= "mov ax,[0x1234]"
    , "88-8b mod=01 1" ~: disasm' "894001" ~?= "mov [bx+si+0x1],ax"
    , "88-8b mod=01 2" ~: disasm' "8949FF" ~?= "mov [bx+di-0x1],cx"
    , "88-8b mod=01 3" ~: disasm' "895202" ~?= "mov [bp+si+0x2],dx"
    , "88-8b mod=01 4" ~: disasm' "895BFE" ~?= "mov [bp+di-0x2],bx"
    , "88-8b mod=01 5" ~: disasm' "896464" ~?= "mov [si+0x64],sp"
    , "88-8b mod=01 6" ~: disasm' "896D9C" ~?= "mov [di-0x64],bp"
    , "88-8b mod=01 7" ~: disasm' "897600" ~?= "mov [bp+0x0],si"
    , "88-8b mod=01 8" ~: disasm' "897601" ~?= "mov [bp+0x1],si"
    , "88-8b mod=01 9" ~: disasm' "897F01" ~?= "mov [bx+0x1],di"
    , "88-8b mod=01 10" ~: disasm' "897FFF" ~?= "mov [bx-0x1],di"
    , "88-8b mod=01 11" ~: disasm' "897F81" ~?= "mov [bx-0x7f],di"
    , "88-8b mod=10 1" ~: disasm' "89800001" ~?= "mov [bx+si+0x100],ax"
    , "88-8b mod=10 2" ~: disasm' "898900FF" ~?= "mov [bx+di-0x100],cx"
    , "88-8b mod=10 3" ~: disasm' "89920002" ~?= "mov [bp+si+0x200],dx"
    , "88-8b mod=10 4" ~: disasm' "899B00FE" ~?= "mov [bp+di-0x200],bx"
    , "88-8b mod=10 5" ~: disasm' "89A40064" ~?= "mov [si+0x6400],sp"
    , "88-8b mod=10 6" ~: disasm' "89AD009C" ~?= "mov [di-0x6400],bp"
    , "88-8b mod=10 7" ~: disasm' "89B60000" ~?= "mov [bp+0x0],si"
    , "88-8b mod=10 8" ~: disasm' "89B60001" ~?= "mov [bp+0x100],si"
    , "88-8b mod=10 9" ~: disasm' "89BF0001" ~?= "mov [bx+0x100],di"
    , "disp16 1" ~: disp16 0      ~?= "+0x0"
    , "disp16 2" ~: disp16 0x7fff ~?= "+0x7fff"
    , "disp16 3" ~: disp16 0x8000 ~?= "-0x8000"
    , "disp16 4" ~: disp16 0xffff ~?= "-0x1"
    , "88-8b mod=11,w=1 1" ~: disasm' "89C0" ~?= "mov ax,ax"
    , "88-8b mod=11,w=1 2" ~: disasm' "89C1" ~?= "mov cx,ax"
    , "88-8b mod=11,w=1 3" ~: disasm' "89C2" ~?= "mov dx,ax"
    , "88-8b mod=11,w=1 4" ~: disasm' "89C3" ~?= "mov bx,ax"
    , "88-8b mod=11,w=1 5" ~: disasm' "89C4" ~?= "mov sp,ax"
    , "88-8b mod=11,w=1 6" ~: disasm' "89C5" ~?= "mov bp,ax"
    , "88-8b mod=11,w=1 7" ~: disasm' "89C6" ~?= "mov si,ax"
    , "88-8b mod=11,w=1 8" ~: disasm' "89C7" ~?= "mov di,ax"
    , "88-8b mod=11,w=0 1" ~: disasm' "88C0" ~?= "mov al,al"
    , "88-8b mod=11,w=0 2" ~: disasm' "88C1" ~?= "mov cl,al"
    , "88-8b mod=11,w=0 3" ~: disasm' "88C2" ~?= "mov dl,al"
    , "88-8b mod=11,w=0 4" ~: disasm' "88C3" ~?= "mov bl,al"
    , "88-8b mod=11,w=0 5" ~: disasm' "88C4" ~?= "mov ah,al"
    , "88-8b mod=11,w=0 6" ~: disasm' "88C5" ~?= "mov ch,al"
    , "88-8b mod=11,w=0 7" ~: disasm' "88C6" ~?= "mov dh,al"
    , "88-8b mod=11,w=0 8" ~: disasm' "88C7" ~?= "mov bh,al"
    , "c6-c7 mod=00,w=0 1" ~: disasm' "C60012" ~?= "mov byte [bx+si],0x12"
    , "c6-c7 mod=00,w=1 1" ~: disasm' "C7001234" ~?= "mov word [bx+si],0x3412"
    ]

main = do
    runTestText (putTextToHandle stderr False) (TestList [testHex, testDisAsm])
