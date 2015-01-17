module Main where

import Test.HUnit
import System.IO
import Data.Char
import Data.Bits
import Hex
import DisAsm

import System.Environment
import qualified Data.ByteString

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
        , "fromLE 0" ~: fromLE 1 [0x12]                   ~?= 0x12
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
    [ "b8 1" ~: disasm 0 [0xb8, 0, 0]       ~?= (3, "mov ax,0x0")
    , "b8 2" ~: disasm 0 [0xb8, 0x34, 0x12] ~?= (3, "mov ax,0x1234")
    , "b8 3" ~: disasm 0 [0xb8, 0x78, 0x56] ~?= (3, "mov ax,0x5678")
    , "b0 1" ~: disasm 0 [0xb0, 0x00] ~?= (2, "mov al,0x0")
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
    , "getByte 0" ~: getByte (1,0,1,1,1,1,0,1) ~?= 0xbd
    , "getByte 1" ~: getByte (1,0,1,1,1,0,0,0) ~?= 0xb8
    , "getByte 2" ~: getByte (1,0,1,1,1,0,0,1) ~?= 0xb9
    , "getReg 0" ~: getReg 1 0 1 ~?= 5
    , "getReg 1" ~: getReg 1 1 1 ~?= 7
    , "getReg 2" ~: getReg 0 0 1 ~?= 1
    , "disp8 1" ~: disp8 0    ~?= "+0x0"
    , "disp8 2" ~: disp8 0x7f ~?= "+0x7f"
    , "disp8 3" ~: disp8 0x80 ~?= "-0x80"
    , "disp8 4" ~: disp8 0xff ~?= "-0x1"
    , "disp16 1" ~: disp16 0      ~?= "+0x0"
    , "disp16 2" ~: disp16 0x7fff ~?= "+0x7fff"
    , "disp16 3" ~: disp16 0x8000 ~?= "-0x8000"
    , "disp16 4" ~: disp16 0xffff ~?= "-0x1"
    , "disAsmB 0" ~: disasmB 0 (1,0,1,1,1,0,0,0) [0, 0] ~?= (3, "mov ax,0x0")
    , "disAsmB 1" ~: disasmB 0 (1,0,0,0,1,0,1,1) [0, 0] ~?= (2, "mov ax,[bx+si]")
    , "disAsmB 2" ~: disasmB 0 (1,0,0,0,1,0,0,1) [0, 0] ~?= (2, "mov [bx+si],ax")
    , "disAsmB 3" ~: disasmB 0 (1,0,0,0,1,0,0,0) [0, 0] ~?= (2, "mov [bx+si],al")
    , "disAsmB 4" ~: disasmB 0 (1,0,0,0,1,0,0,0) [2, 0] ~?= (2, "mov [bp+si],al")
    , "88-8b mod=00 1" ~: disasm' "8900" ~?= "mov [bx+si],ax"
    , "88-8b mod=00 2" ~: disasm' "8909" ~?= "mov [bx+di],cx"
    , "88-8b mod=00 3" ~: disasm' "8912" ~?= "mov [bp+si],dx"
    , "88-8b mod=00 4" ~: disasm' "891b" ~?= "mov [bp+di],bx"
    , "88-8b mod=00 5" ~: disasm' "8924" ~?= "mov [si],sp"
    , "88-8b mod=00 6" ~: disasm' "892d" ~?= "mov [di],bp"
    , "88-8b mod=00 7" ~: disasm' "893f" ~?= "mov [bx],di"
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
    , "88-8b mod=10 10" ~: disasm' "89BF1234" ~?= "mov [bx+0x3412],di"
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
    , "c6-c7 mod=00,w=0 2" ~: disasm' "C60112" ~?= "mov byte [bx+di],0x12"
    , "c6-c7 mod=00,w=0 3" ~: disasm' "C60212" ~?= "mov byte [bp+si],0x12"
    , "c6-c7 mod=01,w=0 1" ~: disasm' "C6401234" ~?= "mov byte [bx+si+0x12],0x34"
    , "c6-c7 mod=01,w=0 2" ~: disasm' "C6411234" ~?= "mov byte [bx+di+0x12],0x34"
    , "c6-c7 mod=01,w=0 3" ~: disasm' "C6421234" ~?= "mov byte [bp+si+0x12],0x34"
    , "c6-c7 mod=10,w=0 1" ~: disasm' "C680123456" ~?= "mov byte [bx+si+0x3412],0x56"
    , "c6-c7 mod=10,w=0 2" ~: disasm' "C681123456" ~?= "mov byte [bx+di+0x3412],0x56"
    , "c6-c7 mod=10,w=0 3" ~: disasm' "C682123456" ~?= "mov byte [bp+si+0x3412],0x56"
    , "c6-c7 mod=00,w=1 1" ~: disasm' "C7001234" ~?= "mov word [bx+si],0x3412"
    , "c6-c7 mod=00,w=1 2" ~: disasm' "C700FF34" ~?= "mov word [bx+si],0x34ff"
    , "c6-c7 mod=01,w=1 1" ~: disasm' "C740123456" ~?= "mov word [bx+si+0x12],0x5634"
    , "c6-c7 mod=01,w=1 2" ~: disasm' "C740FF1234" ~?= "mov word [bx+si-0x1],0x3412"
    , "c6-c7 mod=01,w=1 3" ~: disasm' "C740FF3456" ~?= "mov word [bx+si-0x1],0x5634"
    , "c6-c7 mod=10,w=1 1" ~: disasm' "C78012345678" ~?= "mov word [bx+si+0x3412],0x7856"
    , "c6-c7 mod=10,w=1 2" ~: disasm' "C780FFFFFFFF" ~?= "mov word [bx+si-0x1],0xffff"
    , "c6-c7 mod=10,w=1 2" ~: disasm' "C781FFFFFFFF" ~?= "mov word [bx+di-0x1],0xffff"
    , "c6-c7 mod=11,w=0 1" ~: disasm' "C6C012" ~?= "mov al,0x12"
    , "c6-c7 mod=11,w=1 1" ~: disasm' "C7C01234" ~?= "mov ax,0x3412"
    , "a0-a1 w=0" ~: disasm' "A01234" ~?= "mov al,[0x3412]"
    , "a0-a1 w=1" ~: disasm' "A11234" ~?= "mov ax,[0x3412]"
    , "a2-a3 w=0" ~: disasm' "A21234" ~?= "mov [0x3412],al"
    , "a2-a3 w=1" ~: disasm' "A31234" ~?= "mov [0x3412],ax"
    , "8e mod=00 1" ~: disasm' "8E00" ~?= "mov es,[bx+si]"
    , "8e mod=00 2" ~: disasm' "8E08" ~?= "mov cs,[bx+si]"
    , "8e mod=00 3" ~: disasm' "8E10" ~?= "mov ss,[bx+si]"
    , "8e mod=00 4" ~: disasm' "8E18" ~?= "mov ds,[bx+si]"
    , "8e mod=01 1" ~: disasm' "8E4012" ~?= "mov es,[bx+si+0x12]"
    , "8e mod=01 2" ~: disasm' "8E47FF" ~?= "mov es,[bx-0x1]"
    , "8e mod=10 1" ~: disasm' "8E801234" ~?= "mov es,[bx+si+0x3412]"
    , "8e mod=10 2" ~: disasm' "8E891234" ~?= "mov cs,[bx+di+0x3412]"
    , "8e mod=11 1" ~: disasm' "8EC0" ~?= "mov es,ax"
    , "8e mod=11 2" ~: disasm' "8EC1" ~?= "mov es,cx"
    , "8e mod=11 3" ~: disasm' "8ED0" ~?= "mov ss,ax"
    , "8e mod=11 4" ~: disasm' "8ED8" ~?= "mov ds,ax"
    , "8c mod=00 1" ~: disasm' "8C00" ~?= "mov [bx+si],es"
    , "8c mod=00 2" ~: disasm' "8C08" ~?= "mov [bx+si],cs"
    , "8c mod=00 3" ~: disasm' "8C10" ~?= "mov [bx+si],ss"
    , "8c mod=00 4" ~: disasm' "8C18" ~?= "mov [bx+si],ds"
    , "8c mod=01 1" ~: disasm' "8C4012" ~?= "mov [bx+si+0x12],es"
    , "8c mod=01 2" ~: disasm' "8C47FF" ~?= "mov [bx-0x1],es"
    , "8c mod=10 1" ~: disasm' "8C801234" ~?= "mov [bx+si+0x3412],es"
    , "8c mod=10 2" ~: disasm' "8C891234" ~?= "mov [bx+di+0x3412],cs"
    , "8c mod=11 1" ~: disasm' "8CC0" ~?= "mov ax,es"
    , "8c mod=11 2" ~: disasm' "8CC1" ~?= "mov cx,es"
    , "8c mod=11 3" ~: disasm' "8CD0" ~?= "mov ax,ss"
    , "8c mod=11 4" ~: disasm' "8CD8" ~?= "mov ax,ds"
    , "b8 1" ~: disasm 0 [0xb8, 0, 0]       ~?= (3, "mov ax,0x0")
    , "b8 2" ~: disasm 0 [0xb8, 0x34, 0x12] ~?= (3, "mov ax,0x1234")
    , "disasms" ~: disasms 0 [0xc6, 0x47, 1, 1, 0xb0, 1]
        ~?= [(4, "mov byte [bx+0x1],0x1"), (2, "mov al,0x1")]
    , "disasms' 0" ~: disasms' "C6470101B001"
        ~?= ["mov byte [bx+0x1],0x1", "mov al,0x1"]
    , "disasms' 1" ~: disasms' "C6470101B001" ~?= ["mov byte [bx+0x1],0x1", "mov al,0x1"]
    , "disasms' 2" ~: disasms' "B001" ~?= ["mov al,0x1"]
    , "disasms' 3" ~: disasms' "C6470101B0018CD8" ~?= ["mov byte [bx+0x1],0x1", "mov al,0x1", "mov ax,ds"]
    , "wrong disasm' 1" ~: disasm' "C6470101B001" ~?= "length? 4"
    , "wrong disasm' 2" ~: disasm' "8CD88C801234C6470101B001" ~?= "length? 2"
    , "ndisasm 1" ~: ndisasm 0 [0xc6, 0x47, 1, 1]
        ~?= (4, "00000000  C6470101          mov byte [bx+0x1],0x1")
    , "ndisasm 2" ~: ndisasm 0 [0xb8, 0x34, 0x12]
        ~?= (3, "00000000  B83412            mov ax,0x1234")
    , "ndisasms 1" ~: ndisasms 0 [0xc6, 0x47, 1, 1, 0xb0, 1]
        ~?= [ "00000000  C6470101          mov byte [bx+0x1],0x1"
            , "00000004  B001              mov al,0x1"
            ]
    , "ndisasms 2" ~: ndisasms 0 [0xc6, 0x47, 1, 1, 0xb0, 1, 0xc6, 0x47, 1, 1]
        ~?= [ "00000000  C6470101          mov byte [bx+0x1],0x1"
            , "00000004  B001              mov al,0x1"
            , "00000006  C6470101          mov byte [bx+0x1],0x1"
            ]
    -- push Register/Memory
    , "ff mod=00 1," ~: disasm' "ff30"    ~?= "push word [bx+si]"
    , "ff mod=00 2" ~: disasm' "ff31"     ~?= "push word [bx+di]"
    , "ff mod=00 3" ~: disasm' "ff32"     ~?= "push word [bp+si]"
    , "ff mod=00 4" ~: disasm' "ff33"     ~?= "push word [bp+di]"
    , "ff mod=01 1" ~: disasm' "ff7000"   ~?= "push word [bx+si+0x0]"
    , "ff mod=01 2" ~: disasm' "ff70ff"   ~?= "push word [bx+si-0x1]"
    , "ff mod=01 3" ~: disasm' "ff7700"   ~?= "push word [bx+0x0]"
    , "ff mod=01 4" ~: disasm' "ff77ff"   ~?= "push word [bx-0x1]"
    , "ff mod=10 1" ~: disasm' "ffb00000" ~?= "push word [bx+si+0x0]"
    , "ff mod=10 2" ~: disasm' "ffb0ffff" ~?= "push word [bx+si-0x1]"
    , "ff mod=10 3" ~: disasm' "ffb70000" ~?= "push word [bx+0x0]"
    , "ff mod=10 4" ~: disasm' "ffb7ffff" ~?= "push word [bx-0x1]"
    , "ff mod=11 1" ~: disasm' "fff0"     ~?= "push ax"
    , "ff mod=11 2" ~: disasm' "fff1"     ~?= "push cx"
    , "ff mod=11 3" ~: disasm' "fff2"     ~?= "push dx"
    , "ff mod=11 4" ~: disasm' "fff3"     ~?= "push bx"
    -- push Register
    , "5 1" ~: disasm' "50" ~?= "push ax"
    , "5 2" ~: disasm' "51" ~?= "push cx"
    , "5 3" ~: disasm' "52" ~?= "push dx"
    , "5 4" ~: disasm' "53" ~?= "push bx"
    -- push Segment Register
    , "06-1e 1" ~: disasm' "06" ~?= "push es"
    , "06-1e 2" ~: disasm' "0e" ~?= "push cs"
    , "06-1e 3" ~: disasm' "16" ~?= "push ss"
    , "06-1e 4" ~: disasm' "1e" ~?= "push ds"
    -- push byte
    , "6a 1" ~: disasm' "6a12" ~?= "push byte +0x12"
    , "6a 2" ~: disasm' "6aff" ~?= "push byte -0x1"
    -- pop Register/Memory
    , "8f mod=00 1" ~: disasm' "8f00"     ~?= "pop word [bx+si]"
    , "8f mod=00 2" ~: disasm' "8f01"     ~?= "pop word [bx+di]"
    , "8f mod=00 3" ~: disasm' "8f02"     ~?= "pop word [bp+si]"
    , "8f mod=00 4" ~: disasm' "8f061234" ~?= "pop word [0x3412]"
    , "8f mod=01 1" ~: disasm' "8f4012"   ~?= "pop word [bx+si+0x12]"
    , "8f mod=01 2" ~: disasm' "8f40ff"   ~?= "pop word [bx+si-0x1]"
    , "8f mod=01 3" ~: disasm' "8f4712"   ~?= "pop word [bx+0x12]"
    , "8f mod=01 4" ~: disasm' "8f47ff"   ~?= "pop word [bx-0x1]"
    , "8f mod=10 1" ~: disasm' "8f801234" ~?= "pop word [bx+si+0x3412]"
    , "8f mod=10 2" ~: disasm' "8f80ffff" ~?= "pop word [bx+si-0x1]"
    , "8f mod=10 3" ~: disasm' "8f871234" ~?= "pop word [bx+0x3412]"
    , "8f mod=10 4" ~: disasm' "8f87ffff" ~?= "pop word [bx-0x1]"
    , "8f mod=11 1" ~: disasm' "8fc0"     ~?= "pop ax"
    , "8f mod=11 2" ~: disasm' "8fc1"     ~?= "pop cx"
    , "8f mod=11 3" ~: disasm' "8fc2"     ~?= "pop dx"
    , "8f mod=11 4" ~: disasm' "8fc3"     ~?= "pop bx"
    -- pop Register
    , "5 1" ~: disasm' "58" ~?= "pop ax"
    , "5 2" ~: disasm' "59" ~?= "pop cx"
    , "5 3" ~: disasm' "5a" ~?= "pop dx"
    , "5 4" ~: disasm' "5b" ~?= "pop bx"
    -- pop Segment Register
    , "0-1 1" ~: disasm' "07" ~?= "pop es"
    , "0-1 2" ~: disasm' "0F" ~?= "pop cs"
    , "0-1 3" ~: disasm' "17" ~?= "pop ss"
    , "0-1 4" ~: disasm' "1F" ~?= "pop ds"
    -- xchg Register/Memory with Register
    , "86-87 mod=00,w=0 1" ~: disasm' "8600"     ~?= "xchg al,[bx+si]"
    , "86-87 mod=00,w=0 2" ~: disasm' "863F"     ~?= "xchg bh,[bx]"
    , "86-87 mod=01,w=0 1" ~: disasm' "864012"   ~?= "xchg al,[bx+si+0x12]"
    , "86-87 mod=01,w=0 2" ~: disasm' "867F12"   ~?= "xchg bh,[bx+0x12]"
    , "86-87 mod=10,w=0 1" ~: disasm' "86801234" ~?= "xchg al,[bx+si+0x3412]"
    , "86-87 mod=10,w=0 2" ~: disasm' "868FFFFF" ~?= "xchg cl,[bx-0x1]"
    , "86-87 mod=11,w=0 1" ~: disasm' "86C0"     ~?= "xchg al,al"
    , "86-87 mod=11,w=0 2" ~: disasm' "86FF"     ~?= "xchg bh,bh"
    , "86-87 mod=00,w=1 1" ~: disasm' "8700"     ~?= "xchg ax,[bx+si]"
    , "86-87 mod=00,w=1 2" ~: disasm' "873F"     ~?= "xchg di,[bx]"
    , "86-87 mod=01,w=1 1" ~: disasm' "874012"   ~?= "xchg ax,[bx+si+0x12]"
    , "86-87 mod=01,w=1 2" ~: disasm' "877F12"   ~?= "xchg di,[bx+0x12]"
    , "86-87 mod=10,w=1 1" ~: disasm' "87801234" ~?= "xchg ax,[bx+si+0x3412]"
    , "86-87 mod=10,w=1 2" ~: disasm' "87BFFFFF" ~?= "xchg di,[bx-0x1]"
    , "86-87 mod=11,w=0 1" ~: disasm' "87C0"     ~?= "xchg ax,ax"
    , "86-87 mod=11,w=1 1" ~: disasm' "87FF"     ~?= "xchg di,di"
    -- xchg Register with Accumulator
    , "9 1" ~: disasm' "90" ~?= "nop"
    , "9 2" ~: disasm' "91" ~?= "xchg ax,cx"
    , "9 3" ~: disasm' "92" ~?= "xchg ax,dx"
    , "9 4" ~: disasm' "93" ~?= "xchg ax,bx"
    , "9 5" ~: disasm' "94" ~?= "xchg ax,sp"
    , "9 6" ~: disasm' "95" ~?= "xchg ax,bp"
    , "9 7" ~: disasm' "96" ~?= "xchg ax,si"
    , "9 8" ~: disasm' "97" ~?= "xchg ax,di"
    -- in Fixed Port
    , "e4-e5 w=0 1" ~: disasm' "E412" ~?= "in al,0x12"
    , "e4-e5 w=0 2" ~: disasm' "E4FF" ~?= "in al,0xff"
    , "e4-e5 w=1 3" ~: disasm' "E512" ~?= "in ax,0x12"
    , "e4-e5 w=1 4" ~: disasm' "E5FF" ~?= "in ax,0xff"
    -- in Variable Port
    , "ec-ed 1" ~: disasm' "EC" ~?= "in al,dx"
    , "ec-ed 2" ~: disasm' "ED" ~?= "in ax,dx"
    -- out Fixed Port
    , "e6-e7 w=0 1" ~: disasm' "E612" ~?= "out 0x12,al"
    , "e6-e7 w=0 2" ~: disasm' "E6FF" ~?= "out 0xff,al"
    , "e6-e7 w=1 3" ~: disasm' "E712" ~?= "out 0x12,ax"
    , "e6-e7 w=1 4" ~: disasm' "E7FF" ~?= "out 0xff,ax"
    -- xlat
    , "d7 1" ~: disasm' "D7" ~?= "xlatb"
    -- lea
    , "8d 1" ~: disasm' "8D00"     ~?= "lea ax,[bx+si]"
    , "8d 2" ~: disasm' "8D4012"   ~?= "lea ax,[bx+si+0x12]"
    , "8d 3" ~: disasm' "8D801234" ~?= "lea ax,[bx+si+0x3412]"
    , "8d 4" ~: disasm' "8DC0"     ~?= "lea ax,ax"
    -- lds
    , "c5 1" ~: disasm' "C500"     ~?= "lds ax,[bx+si]"
    , "c5 2" ~: disasm' "C54012"   ~?= "lds ax,[bx+si+0x12]"
    , "c5 3" ~: disasm' "C5801234" ~?= "lds ax,[bx+si+0x3412]"
    , "c5 4" ~: disasm' "C5C0"     ~?= "lds ax,ax"
    -- les
    , "c4 1" ~: disasm' "C400"     ~?= "les ax,[bx+si]"
    , "c4 2" ~: disasm' "C44012"   ~?= "les ax,[bx+si+0x12]"
    , "c4 3" ~: disasm' "C4801234" ~?= "les ax,[bx+si+0x3412]"
    , "c4 4" ~: disasm' "C4C0"     ~?= "les ax,ax"
    -- lahf
    , "9f 1" ~: disasm' "9f" ~?= "lahf"
    -- sahf
    , "9e 1" ~: disasm' "9e" ~?= "sahf"
    -- pushf
    , "9c 1" ~: disasm' "9c" ~?= "pushfw"
    -- popf
    , "9d 1" ~: disasm' "9d" ~?= "popfw"
    -- add Reg./Memory with Register to Either
    , "00-03 d=0,w=0" ~: disasm' "0000" ~?= "add [bx+si],al"
    , "00-03 d=0,w=1" ~: disasm' "0100" ~?= "add [bx+si],ax"
    , "00-03 d=1,w=0" ~: disasm' "0200" ~?= "add al,[bx+si]"
    , "00-03 d=1,w=1" ~: disasm' "0300" ~?= "add ax,[bx+si]"
    -- add Immediate to Register/Memory
    , "80-83 s=0,w=0" ~: disasm' "800012"   ~?= "add byte [bx+si],0x12"
    , "80-83 s=0,w=1" ~: disasm' "81001234" ~?= "add word [bx+si],0x3412"
    , "80-83 s=1,w=0" ~: disasm' "82"   ~?= "db 0x82"
    , "80-83 s=1,w=1" ~: disasm' "830012"   ~?= "add word [bx+si],byte +0x12"
    -- add Immediate to Accumulator
    , "04-05 w=0" ~: disasm' "0412"   ~?= "add al,0x12"
    , "04-05 w=1" ~: disasm' "051234" ~?= "add ax,0x3412"
    -- adc Reg./Memory with Register to Either
    , "10-13 d=0,w=0" ~: disasm' "1000" ~?= "adc [bx+si],al"
    , "10-13 d=0,w=1" ~: disasm' "1100" ~?= "adc [bx+si],ax"
    , "10-13 d=1,w=0" ~: disasm' "1200" ~?= "adc al,[bx+si]"
    , "10-13 d=1,w=1" ~: disasm' "1300" ~?= "adc ax,[bx+si]"
    -- adc Immediate to Register/Memory
    , "8010-83D7 s=0,w=0" ~: disasm' "801012"   ~?= "adc byte [bx+si],0x12"
    , "8010-83D7 s=0,w=1" ~: disasm' "81101234" ~?= "adc word [bx+si],0x3412"
    , "8010-83D7 s=1,w=0" ~: disasm' "82"   ~?= "db 0x82"
    , "8010-83D7 s=1,w=1" ~: disasm' "831012"   ~?= "adc word [bx+si],byte +0x12"
    -- adc Immediate to Accumulator
    , "14-15 w=0" ~: disasm' "1412"   ~?= "adc al,0x12"
    , "14-15 w=1" ~: disasm' "151234" ~?= "adc ax,0x3412"
    -- inc Register/Memory
    , "fe-ff w=0" ~: disasm' "fe00"   ~?= "inc byte [bx+si]"
    , "fe-ff w=1" ~: disasm' "ff00"   ~?= "inc word [bx+si]"
    -- inc Register
    , "40" ~: disasm' "40"   ~?= "inc ax"
    -- aaa
    , "37" ~: disasm' "37"   ~?= "aaa"
    -- daa
    , "27" ~: disasm' "27"   ~?= "daa"
    -- sub Reg./Memory and Register to Either
    , "28-2b d=0,w=0" ~: disasm' "2800" ~?= "sub [bx+si],al"
    , "28-2b d=0,w=1" ~: disasm' "2900" ~?= "sub [bx+si],ax"
    , "28-2b d=1,w=0" ~: disasm' "2a00" ~?= "sub al,[bx+si]"
    , "28-2b d=1,w=1" ~: disasm' "2b00" ~?= "sub ax,[bx+si]"
    -- sub Immediate from Register/Memory
    , "80-83 s=0,w=0" ~: disasm' "802812"   ~?= "sub byte [bx+si],0x12"
    , "80-83 s=0,w=1" ~: disasm' "81281234" ~?= "sub word [bx+si],0x3412"
    , "80-83 s=1,w=0" ~: disasm' "82"   ~?= "db 0x82"
    , "80-83 s=1,w=1" ~: disasm' "832812"   ~?= "sub word [bx+si],byte +0x12"
    -- sub Immediate from Accumulator
    , "2c-2d w=0" ~: disasm' "2c12"   ~?= "sub al,0x12"
    , "2c-2d w=1" ~: disasm' "2d1234" ~?= "sub ax,0x3412"
    -- sbb Reg./Memory and Register to Either
    , "18-1b d=0,w=0" ~: disasm' "1800" ~?= "sbb [bx+si],al"
    , "18-1b d=0,w=1" ~: disasm' "1900" ~?= "sbb [bx+si],ax"
    , "18-1b d=1,w=0" ~: disasm' "1a00" ~?= "sbb al,[bx+si]"
    , "18-1b d=1,w=1" ~: disasm' "1b00" ~?= "sbb ax,[bx+si]"
    -- sbb Immediate from Register/Memory
    , "80-83 s=0,w=0" ~: disasm' "801812"   ~?= "sbb byte [bx+si],0x12"
    , "80-83 s=0,w=1" ~: disasm' "81181234" ~?= "sbb word [bx+si],0x3412"
    , "80-83 s=1,w=0" ~: disasm' "82"   ~?= "db 0x82"
    , "80-83 s=1,w=1" ~: disasm' "831812"   ~?= "sbb word [bx+si],byte +0x12"
    -- sbb Immediate from Accumulator
    , "1c-1d w=0" ~: disasm' "1c12"   ~?= "sbb al,0x12"
    , "1c-1d w=1" ~: disasm' "1d1234" ~?= "sbb ax,0x3412"
    -- dec Register/Memory
    , "fe-ff w=0" ~: disasm' "fe08"   ~?= "dec byte [bx+si]"
    , "fe-ff w=1" ~: disasm' "ff08"   ~?= "dec word [bx+si]"
    -- dec Register
    , "48" ~: disasm' "48"   ~?= "dec ax"
    -- neg
    , "f6-f7 w=0" ~: disasm' "f618"   ~?= "neg byte [bx+si]"
    , "f6-f7 w=1" ~: disasm' "f718"   ~?= "neg word [bx+si]"
    -- cmp Register/Memory and Register
    , "38-3b d=0,w=0" ~: disasm' "3800" ~?= "cmp [bx+si],al"
    , "38-3b d=0,w=1" ~: disasm' "3900" ~?= "cmp [bx+si],ax"
    , "38-3b d=1,w=0" ~: disasm' "3a00" ~?= "cmp al,[bx+si]"
    , "38-3b d=1,w=1" ~: disasm' "3b00" ~?= "cmp ax,[bx+si]"
    -- cmp Immediate with Register/Memory
    , "80-83 s=0,w=0" ~: disasm' "803812"   ~?= "cmp byte [bx+si],0x12"
    , "80-83 s=0,w=1" ~: disasm' "81381234" ~?= "cmp word [bx+si],0x3412"
    , "80-83 s=1,w=0" ~: disasm' "82"   ~?= "db 0x82"
    , "80-83 s=1,w=1" ~: disasm' "833812"   ~?= "cmp word [bx+si],byte +0x12"
    -- cmp Immediate with Accumulator
    , "3c-3d w=0" ~: disasm' "3c12"   ~?= "cmp al,0x12"
    , "3c-3d w=1" ~: disasm' "3d1234" ~?= "cmp ax,0x3412"
    -- aas
    , "3f" ~: disasm' "3f"   ~?= "aas"
    -- das
    , "2f" ~: disasm' "2f"   ~?= "das"
    -- mul
    , "f6-f7 w=0" ~: disasm' "f620"   ~?= "mul byte [bx+si]"
    , "f6-f7 w=1" ~: disasm' "f720"   ~?= "mul word [bx+si]"
    -- imul
    , "f6-f7 w=0" ~: disasm' "f628"   ~?= "imul byte [bx+si]"
    , "f6-f7 w=1" ~: disasm' "f728"   ~?= "imul word [bx+si]"
    -- aam
    , "d40a" ~: disasm' "d40a"   ~?= "aam"
    -- div
    , "f6-f7 w=0" ~: disasm' "f630"   ~?= "div byte [bx+si]"
    , "f6-f7 w=1" ~: disasm' "f730"   ~?= "div word [bx+si]"
    -- idiv
    , "f6-f7 w=0" ~: disasm' "f638"   ~?= "idiv byte [bx+si]"
    , "f6-f7 w=1" ~: disasm' "f738"   ~?= "idiv word [bx+si]"
    -- aam
    , "d50a" ~: disasm' "d50a"   ~?= "aad"
    -- cbw
    , "98" ~: disasm' "98"   ~?= "cbw"
    -- cwd
    , "99" ~: disasm' "99"   ~?= "cwd"
    -- not
    , "f6-f7 w=0" ~: disasm' "f610"   ~?= "not byte [bx+si]"
    , "f6-f7 w=1" ~: disasm' "f710"   ~?= "not word [bx+si]"
    -- shl/sal
    , "d0-d3 v=0,w=0" ~: disasm' "d020" ~?= "shl byte [bx+si],1"
    , "d0-d3 v=0,w=1" ~: disasm' "d120" ~?= "shl word [bx+si],1"
    , "d0-d3 v=1,w=0" ~: disasm' "d220" ~?= "shl byte [bx+si],cl"
    , "d0-d3 v=1,w=1" ~: disasm' "d320" ~?= "shl word [bx+si],cl"
    -- shr
    , "d0-d3 v=0,w=0" ~: disasm' "d028" ~?= "shr byte [bx+si],1"
    , "d0-d3 v=0,w=1" ~: disasm' "d128" ~?= "shr word [bx+si],1"
    , "d0-d3 v=1,w=0" ~: disasm' "d228" ~?= "shr byte [bx+si],cl"
    , "d0-d3 v=1,w=1" ~: disasm' "d328" ~?= "shr word [bx+si],cl"
    -- sar
    , "d0-d3 v=0,w=0" ~: disasm' "d038" ~?= "sar byte [bx+si],1"
    , "d0-d3 v=0,w=1" ~: disasm' "d138" ~?= "sar word [bx+si],1"
    , "d0-d3 v=1,w=0" ~: disasm' "d238" ~?= "sar byte [bx+si],cl"
    , "d0-d3 v=1,w=1" ~: disasm' "d338" ~?= "sar word [bx+si],cl"
    -- rol
    , "d0-d3 v=0,w=0" ~: disasm' "d000" ~?= "rol byte [bx+si],1"
    , "d0-d3 v=0,w=1" ~: disasm' "d100" ~?= "rol word [bx+si],1"
    , "d0-d3 v=1,w=0" ~: disasm' "d200" ~?= "rol byte [bx+si],cl"
    , "d0-d3 v=1,w=1" ~: disasm' "d300" ~?= "rol word [bx+si],cl"
    -- ror
    , "d0-d3 v=0,w=0" ~: disasm' "d008" ~?= "ror byte [bx+si],1"
    , "d0-d3 v=0,w=1" ~: disasm' "d108" ~?= "ror word [bx+si],1"
    , "d0-d3 v=1,w=0" ~: disasm' "d208" ~?= "ror byte [bx+si],cl"
    , "d0-d3 v=1,w=1" ~: disasm' "d308" ~?= "ror word [bx+si],cl"
    -- rcl
    , "d0-d3 v=0,w=0" ~: disasm' "d010" ~?= "rcl byte [bx+si],1"
    , "d0-d3 v=0,w=1" ~: disasm' "d110" ~?= "rcl word [bx+si],1"
    , "d0-d3 v=1,w=0" ~: disasm' "d210" ~?= "rcl byte [bx+si],cl"
    , "d0-d3 v=1,w=1" ~: disasm' "d310" ~?= "rcl word [bx+si],cl"
    -- rcr
    , "d0-d3 v=0,w=0" ~: disasm' "d018" ~?= "rcr byte [bx+si],1"
    , "d0-d3 v=0,w=1" ~: disasm' "d118" ~?= "rcr word [bx+si],1"
    , "d0-d3 v=1,w=0" ~: disasm' "d218" ~?= "rcr byte [bx+si],cl"
    , "d0-d3 v=1,w=1" ~: disasm' "d318" ~?= "rcr word [bx+si],cl"
    -- and Reg./Memory and Register to Either
    , "20-23 d=0,w=0" ~: disasm' "2000" ~?= "and [bx+si],al"
    , "20-23 d=0,w=1" ~: disasm' "2100" ~?= "and [bx+si],ax"
    , "20-23 d=1,w=0" ~: disasm' "2200" ~?= "and al,[bx+si]"
    , "20-23 d=1,w=1" ~: disasm' "2300" ~?= "and ax,[bx+si]"
    -- and Immediate to Register/Memory
    , "80-81 w=0" ~: disasm' "802012"   ~?= "and byte [bx+si],0x12"
    , "80-81 w=1" ~: disasm' "81201234" ~?= "and word [bx+si],0x3412"
    -- and Immediate to Accumulator
    , "24-25 w=0" ~: disasm' "2412"   ~?= "and al,0x12"
    , "24-25 w=1" ~: disasm' "251234" ~?= "and ax,0x3412"
    -- test Register/Memory and Register
    , "84-85 w=0" ~: disasm' "8400" ~?= "test [bx+si],al"
    , "84-85 w=1" ~: disasm' "8500" ~?= "test [bx+si],ax"
    -- test Immediate Data and Register/Memory
    , "f6-f7 w=0" ~: disasm' "f60012"   ~?= "test byte [bx+si],0x12"
    , "f6-f7 w=1" ~: disasm' "f7001234" ~?= "test word [bx+si],0x3412"
    -- test Immediate Data and Accumulator
    , "a8-a9 w=0" ~: disasm' "a812"   ~?= "test al,0x12"
    , "a8-a9 w=1" ~: disasm' "a91234" ~?= "test ax,0x3412"
    -- or Reg./Memory and Register to Either
    , "08-0b d=0,w=0" ~: disasm' "0800" ~?= "or [bx+si],al"
    , "08-0b d=0,w=1" ~: disasm' "0900" ~?= "or [bx+si],ax"
    , "08-0b d=1,w=0" ~: disasm' "0a00" ~?= "or al,[bx+si]"
    , "08-0b d=1,w=1" ~: disasm' "0b00" ~?= "or ax,[bx+si]"
    -- or Immediate to Register/Memory
    , "80-81 w=0" ~: disasm' "800812"   ~?= "or byte [bx+si],0x12"
    , "80-81 w=1" ~: disasm' "81081234" ~?= "or word [bx+si],0x3412"
    -- or Immediate to Accumulator
    , "0c-0d w=0" ~: disasm' "0c12"   ~?= "or al,0x12"
    , "0c-0d w=1" ~: disasm' "0d1234" ~?= "or ax,0x3412"
    -- xor Reg./Memory and Register to Either
    , "30-33 d=0,w=0" ~: disasm' "3000" ~?= "xor [bx+si],al"
    , "30-33 d=0,w=1" ~: disasm' "3100" ~?= "xor [bx+si],ax"
    , "30-33 d=1,w=0" ~: disasm' "3200" ~?= "xor al,[bx+si]"
    , "30-33 d=1,w=1" ~: disasm' "3300" ~?= "xor ax,[bx+si]"
    -- xor Immediate to Register/Memory
    , "80-81 w=0" ~: disasm' "803012"   ~?= "xor byte [bx+si],0x12"
    , "80-81 w=1" ~: disasm' "81301234" ~?= "xor word [bx+si],0x3412"
    -- xor Immediate to Accumulator
    , "34-35 w=0" ~: disasm' "3412"   ~?= "xor al,0x12"
    , "34-35 w=1" ~: disasm' "351234" ~?= "xor ax,0x3412"
    -- rep
    , "f2-f3 z=0" ~: disasm' "f20000" ~?= "repne add [bx+si],al"
    , "f2-f3 z=1" ~: disasm' "f30000" ~?= "rep add [bx+si],al"
    -- movs
    , "a4-a5 w=0" ~: disasm' "a4" ~?= "movsb"
    , "a4-a5 w=1" ~: disasm' "a5" ~?= "movsw"
    -- cmp
    , "a6-a7 w=0" ~: disasm' "a6" ~?= "cmpsb"
    , "a6-a7 w=1" ~: disasm' "a7" ~?= "cmpsw"
    -- scas
    , "ae-af w=0" ~: disasm' "ae" ~?= "scasb"
    , "ae-af w=1" ~: disasm' "af" ~?= "scasw"
    -- lods
    , "ac-ad w=0" ~: disasm' "ac" ~?= "lodsb"
    , "ac-ad w=1" ~: disasm' "ad" ~?= "lodsw"
    -- stos
    , "aa-ab w=0" ~: disasm' "aa" ~?= "stosb"
    , "aa-ab w=1" ~: disasm' "ab" ~?= "stosw"
    -- call Direct within Segment
    , "e8 1" ~: disasm' "e80012" ~?= "call word 0x1203"
    , "e8 2" ~: disasm 0 [0xe8, 0, 0x12] ~?= (3, "call word 0x1203")
    , "e8 3" ~: disasm 7072 [0xe8, 0xc8, 0xf9] ~?= (3, "call word 0x156b")
    -- call Indirect within Segment
    , "ff" ~: disasm' "ff10" ~?= "call word [bx+si]"
    -- call Direct Intersegment
    , "9a 1" ~: disasm' "9a12345678" ~?= "call word 0x7856:0x3412"
    , "9a 2" ~: disasm' "9a00120012" ~?= "call word 0x1200:0x1200"
    -- call Indirect Intersegment
    , "ff 1" ~: disasm' "ff18" ~?= "call word far [bx+si]"
    -- call Indirect Intersegment mod=11は実行不可能
    , "ff 2" ~: disasm' "ffd8" ~?= "call word far ax"
    -- jmp Direct within Segment
    , "e9 1" ~: disasm' "e90012" ~?= "jmp word 0x1203"
    , "e9 2" ~: disasm 0 [0xe9, 0, 0x12] ~?= (3, "jmp word 0x1203")
    , "e9 3" ~: disasm 3 [0xe9, 0, 0x12] ~?= (3, "jmp word 0x1206")
    -- jmp Direct within Segment-Short
    , "eb 1" ~: disasm' "eb00" ~?= "jmp short 0x2"
    , "eb 2" ~: disasm 0 [0xeb, 0] ~?= (2, "jmp short 0x2")
    , "eb 3" ~: disasm 3 [0xeb, 0] ~?= (2, "jmp short 0x5")
    , "eb 4" ~: disasm 507 [0xeb, 0xd4] ~?= (2, "jmp short 0x1d1")
    , "eb 5" ~: disasm 7627 [0xeb, 0xaa] ~?= (2, "jmp short 0x1d77")
    , "eb 6" ~: disasm 5061 [0xeb, 0x7] ~?= (2, "jmp short 0x13ce")
    , "eb 7" ~: disasm 5867 [0xeb, 0x5] ~?= (2, "jmp short 0x16f2")
    -- jmp Indirect within Segment
    , "ff" ~: disasm' "ff20" ~?= "jmp word [bx+si]"
    -- jmp Direct Intersegment
    , "ea 1" ~: disasm' "ea12345678" ~?= "jmp word 0x7856:0x3412"
    , "ea 2" ~: disasm' "ea00120012" ~?= "jmp word 0x1200:0x1200"
    -- jmp Indirect Intersegment
    , "ff 1" ~: disasm' "ff28" ~?= "jmp word far [bx+si]"
    -- jmp Indirect Intersegment mod=11は実行不可能
    , "ff 2" ~: disasm' "ffe8" ~?= "jmp word far ax"
    -- ret Within Segment
    , "c3" ~: disasm' "c3" ~?= "ret"
    -- ret Within Seg Adding Immed to SP
    , "c2 1" ~: disasm' "c21234" ~?= "ret 0x3412"
    -- ret Intersegment
    , "cb" ~: disasm' "cb" ~?= "retf"
    -- ret Intersegment Adding Immediate to SP
    , "ca 1" ~: disasm' "ca1234" ~?= "retf 0x3412"
    -- je/jz(jz←je)
    , "74 1" ~: disasm' "7400" ~?= "jz 0x2"
    , "74 2" ~: disasm 0 [0x74, 0] ~?= (2, "jz 0x2")
    , "74 3" ~: disasm 3 [0x74, 0] ~?= (2, "jz 0x5")
    -- jl/jnge(jl←jnge)
    , "7c 1" ~: disasm' "7c00" ~?= "jl 0x2"
    , "7c 2" ~: disasm 0 [0x7c, 0] ~?= (2, "jl 0x2")
    , "7c 3" ~: disasm 3 [0x7c, 0] ~?= (2, "jl 0x5")
    -- jle/jng(jng←jle)
    , "7e 1" ~: disasm' "7e00" ~?= "jng 0x2"
    , "7e 2" ~: disasm 0 [0x7e, 0] ~?= (2, "jng 0x2")
    , "7e 3" ~: disasm 3 [0x7e, 0] ~?= (2, "jng 0x5")
    -- jb/jnae(jc←jb/jnae)
    , "72 1" ~: disasm' "7200" ~?= "jc 0x2"
    , "72 2" ~: disasm 0 [0x72, 0] ~?= (2, "jc 0x2")
    , "72 3" ~: disasm 3 [0x72, 0] ~?= (2, "jc 0x5")
    -- jbe/jna(jna←jbe)
    , "76 1" ~: disasm' "7600" ~?= "jna 0x2"
    , "76 2" ~: disasm 0 [0x76, 0] ~?= (2, "jna 0x2")
    , "76 3" ~: disasm 3 [0x76, 0] ~?= (2, "jna 0x5")
    -- jp/jpe(jpe←jp)
    , "7a 1" ~: disasm' "7a00" ~?= "jpe 0x2"
    , "7a 2" ~: disasm 0 [0x7a, 0] ~?= (2, "jpe 0x2")
    , "7a 3" ~: disasm 3 [0x7a, 0] ~?= (2, "jpe 0x5")
    -- jo
    , "70 1" ~: disasm' "7000" ~?= "jo 0x2"
    , "70 2" ~: disasm 0 [0x70, 0] ~?= (2, "jo 0x2")
    , "70 3" ~: disasm 3 [0x70, 0] ~?= (2, "jo 0x5")
    -- js
    , "78 1" ~: disasm' "7800" ~?= "js 0x2"
    , "78 2" ~: disasm 0 [0x78, 0] ~?= (2, "js 0x2")
    , "78 3" ~: disasm 3 [0x78, 0] ~?= (2, "js 0x5")
    -- jne/jnz(jnz←jne)
    , "75 1" ~: disasm' "7500" ~?= "jnz 0x2"
    , "75 2" ~: disasm 0 [0x75, 0] ~?= (2, "jnz 0x2")
    , "75 3" ~: disasm 3 [0x75, 0] ~?= (2, "jnz 0x5")
    -- jnl/jge(jnl←jge)
    , "7d 1" ~: disasm' "7d00" ~?= "jnl 0x2"
    , "7d 2" ~: disasm 0 [0x7d, 0] ~?= (2, "jnl 0x2")
    , "7d 3" ~: disasm 3 [0x7d, 0] ~?= (2, "jnl 0x5")
    -- jnle/jg(jg←jnle)
    , "7f 1" ~: disasm' "7f00" ~?= "jg 0x2"
    , "7f 2" ~: disasm 0 [0x7f, 0] ~?= (2, "jg 0x2")
    , "7f 3" ~: disasm 3 [0x7f, 0] ~?= (2, "jg 0x5")
    -- jnb/jae(jnc←jnb/jae)
    , "73 1" ~: disasm' "7300" ~?= "jnc 0x2"
    , "73 2" ~: disasm 0 [0x73, 0] ~?= (2, "jnc 0x2")
    , "73 3" ~: disasm 3 [0x73, 0] ~?= (2, "jnc 0x5")
    -- jnbe/ja(ja←jnbe)
    , "77 1" ~: disasm' "7700" ~?= "ja 0x2"
    , "77 2" ~: disasm 0 [0x77, 0] ~?= (2, "ja 0x2")
    , "77 3" ~: disasm 3 [0x77, 0] ~?= (2, "ja 0x5")
    -- jnp/jpo(jpo←jnp)
    , "7b 1" ~: disasm' "7b00" ~?= "jpo 0x2"
    , "7b 2" ~: disasm 0 [0x7b, 0] ~?= (2, "jpo 0x2")
    , "7b 3" ~: disasm 3 [0x7b, 0] ~?= (2, "jpo 0x5")
    -- jno
    , "71 1" ~: disasm' "7100" ~?= "jno 0x2"
    , "71 2" ~: disasm 0 [0x71, 0] ~?= (2, "jno 0x2")
    , "71 3" ~: disasm 3 [0x71, 0] ~?= (2, "jno 0x5")
    -- jns
    , "79 1" ~: disasm' "7900" ~?= "jns 0x2"
    , "79 2" ~: disasm 0 [0x79, 0] ~?= (2, "jns 0x2")
    , "79 3" ~: disasm 3 [0x79, 0] ~?= (2, "jns 0x5")
    -- loop
    , "e2 1" ~: disasm' "e200" ~?= "loop 0x2"
    , "e2 2" ~: disasm 0 [0xe2, 0] ~?= (2, "loop 0x2")
    , "e2 3" ~: disasm 3 [0xe2, 0] ~?= (2, "loop 0x5")
    -- loopz/loope(loope←loopz)
    , "e1 1" ~: disasm' "e100" ~?= "loope 0x2"
    , "e1 2" ~: disasm 0 [0xe1, 0] ~?= (2, "loope 0x2")
    , "e1 3" ~: disasm 3 [0xe1, 0] ~?= (2, "loope 0x5")
    -- loopnz/loopne(loopne←loopnz)
    , "e0 1" ~: disasm' "e000" ~?= "loopne 0x2"
    , "e0 2" ~: disasm 0 [0xe0, 0] ~?= (2, "loopne 0x2")
    , "e0 3" ~: disasm 3 [0xe0, 0] ~?= (2, "loopne 0x5")
    -- jcxz
    , "e3 1" ~: disasm' "e300" ~?= "jcxz 0x2"
    , "e3 2" ~: disasm 0 [0xe3, 0] ~?= (2, "jcxz 0x2")
    , "e3 3" ~: disasm 3 [0xe3, 0] ~?= (2, "jcxz 0x5")
    -- int Type Specified
    , "cd" ~: disasm' "cd12" ~?= "int 0x12"
    -- int Type 3
    , "cc" ~: disasm' "cc" ~?= "int3"
    -- into
    , "ce" ~: disasm' "ce" ~?= "into"
    -- iret(iretw←iret)
    , "cf" ~: disasm' "cf" ~?= "iretw"
    -- clc
    , "f8" ~: disasm' "f8" ~?= "clc"
    -- cmc
    , "f5" ~: disasm' "f5" ~?= "cmc"
    -- stc
    , "f9" ~: disasm' "f9" ~?= "stc"
    -- cld
    , "fc" ~: disasm' "fc" ~?= "cld"
    -- std
    , "fd" ~: disasm' "fd" ~?= "std"
    -- cli
    , "fa" ~: disasm' "fa" ~?= "cli"
    -- sti
    , "fb" ~: disasm' "fb" ~?= "sti"
    -- hlt
    , "f4" ~: disasm' "f4" ~?= "hlt"
    -- wait
    , "9b" ~: disasm' "9b" ~?= "wait"
    -- lock
    , "f0" ~: disasm' "f0" ~?= "lock"
    -- segment override prefix
    , "sop 1" ~: disasm' "268c061234" ~?= "mov [es:0x3412],es"
    , "sop 2" ~: disasm' "2ea21234" ~?= "mov [cs:0x3412],al"
    , "sop 3" ~: disasm' "2eff361234" ~?= "push word [cs:0x3412]"
    -- pushaw
    , "60" ~: disasm' "60" ~?= "pushaw"
    -- push word
    , "68" ~: disasm' "681234" ~?= "push word 0x3412"
    -- popaw
    , "61" ~: disasm' "61" ~?= "popaw"
    ]

main = do
    args <- getArgs
    if args == []
        then do
            runTestText (putTextToHandle stderr False)
                (TestList [testHex, testDisAsm])
            return ()
        else do
            bytes <- Data.ByteString.readFile $ args !! 0
            putStr $ unlines $ ndisasms 0
                [fromIntegral b | b <- Data.ByteString.unpack bytes]
