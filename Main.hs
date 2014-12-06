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
    [ "b8 1" ~: disasm [0xb8, 0, 0]       ~?= (3, "mov ax,0x0")
    , "b8 2" ~: disasm [0xb8, 0x34, 0x12] ~?= (3, "mov ax,0x1234")
    , "b8 3" ~: disasm [0xb8, 0x78, 0x56] ~?= (3, "mov ax,0x5678")
    , "b0 1" ~: disasm [0xb0, 0x00] ~?= (2, "mov al,0x0")
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
    , "disp8 1" ~: disp8 0    ~?= "+0x0"
    , "disp8 2" ~: disp8 0x7f ~?= "+0x7f"
    , "disp8 3" ~: disp8 0x80 ~?= "-0x80"
    , "disp8 4" ~: disp8 0xff ~?= "-0x1"
    , "disp16 1" ~: disp16 0      ~?= "+0x0"
    , "disp16 2" ~: disp16 0x7fff ~?= "+0x7fff"
    , "disp16 3" ~: disp16 0x8000 ~?= "-0x8000"
    , "disp16 4" ~: disp16 0xffff ~?= "-0x1"
    , "disAsmB 0" ~: disasmB (1,0,1,1,1,0,0,0) [0, 0] ~?= (3, "mov ax,0x0")
    , "disAsmB 1" ~: disasmB (1,0,0,0,1,0,1,1) [0, 0] ~?= (2, "mov ax,[bx+si]")
    , "disAsmB 2" ~: disasmB (1,0,0,0,1,0,0,1) [0, 0] ~?= (2, "mov [bx+si],ax")
    , "disAsmB 3" ~: disasmB (1,0,0,0,1,0,0,0) [0, 0] ~?= (2, "mov [bx+si],al")
    , "disAsmB 4" ~: disasmB (1,0,0,0,1,0,0,0) [2, 0] ~?= (2, "mov [bp+si],al")
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
    , "b8 1" ~: disasm [0xb8, 0, 0]       ~?= (3, "mov ax,0x0")
    , "b8 2" ~: disasm [0xb8, 0x34, 0x12] ~?= (3, "mov ax,0x1234")
    , "disasms" ~: disasms [0xc6, 0x47, 1, 1, 0xb0, 1]
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
    , "ff mod=00 1," ~: disasm' "ff30" ~?= "push word [bx+si]"
    , "ff mod=00 2" ~: disasm' "ff31" ~?= "push word [bx+di]"
    , "ff mod=00 3" ~: disasm' "ff32" ~?= "push word [bp+si]"
    , "ff mod=00 4" ~: disasm' "ff33" ~?= "push word [bp+di]"
    , "ff mod=01 1" ~: disasm' "ff7000" ~?= "push word [bx+si+0x0]"
    , "ff mod=01 2" ~: disasm' "ff70ff" ~?= "push word [bx+si-0x1]"
    , "ff mod=01 3" ~: disasm' "ff7700" ~?= "push word [bx+0x0]"
    , "ff mod=01 4" ~: disasm' "ff77ff" ~?= "push word [bx-0x1]"
    , "ff mod=10 1" ~: disasm' "ffb00000" ~?= "push word [bx+si+0x0]"
    , "ff mod=10 2" ~: disasm' "ffb0ffff" ~?= "push word [bx+si-0x1]"
    , "ff mod=10 3" ~: disasm' "ffb70000" ~?= "push word [bx+0x0]"
    , "ff mod=10 4" ~: disasm' "ffb7ffff" ~?= "push word [bx-0x1]"
    , "ff mod=11 1" ~: disasm' "fff0" ~?= "push ax"
    , "ff mod=11 2" ~: disasm' "fff1" ~?= "push cx"
    , "ff mod=11 3" ~: disasm' "fff2" ~?= "push dx"
    , "ff mod=11 4" ~: disasm' "fff3" ~?= "push bx"
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
    -- pop Register/Memory
    , "8f mod=00 1" ~: disasm' "8f00" ~?= "pop word [bx+si]"
    , "8f mod=00 2" ~: disasm' "8f01" ~?= "pop word [bx+di]"
    , "8f mod=00 3" ~: disasm' "8f02" ~?= "pop word [bp+si]"
    , "8f mod=00 4" ~: disasm' "8f061234" ~?= "pop word [0x3412]"
    , "8f mod=01 1" ~: disasm' "8f4012" ~?= "pop word [bx+si+0x12]"
    , "8f mod=01 2" ~: disasm' "8f40ff" ~?= "pop word [bx+si-0x1]"
    , "8f mod=01 3" ~: disasm' "8f4712" ~?= "pop word [bx+0x12]"
    , "8f mod=01 4" ~: disasm' "8f47ff" ~?= "pop word [bx-0x1]"
    , "8f mod=10 1" ~: disasm' "8f801234" ~?= "pop word [bx+si+0x3412]"
    , "8f mod=10 2" ~: disasm' "8f80ffff" ~?= "pop word [bx+si-0x1]"
    , "8f mod=10 3" ~: disasm' "8f871234" ~?= "pop word [bx+0x3412]"
    , "8f mod=10 4" ~: disasm' "8f87ffff" ~?= "pop word [bx-0x1]"
    , "8f mod=11 1" ~: disasm' "8fc0" ~?= "pop ax"
    , "8f mod=11 2" ~: disasm' "8fc1" ~?= "pop cx"
    , "8f mod=11 3" ~: disasm' "8fc2" ~?= "pop dx"
    , "8f mod=11 4" ~: disasm' "8fc3" ~?= "pop bx"
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
    , "86-87 mod=00,w=0 1" ~: disasm' "8600" ~?= "xchg al,[bx+si]"
    , "86-87 mod=00,w=0 2" ~: disasm' "863F" ~?= "xchg bh,[bx]"
    , "86-87 mod=01,w=0 1" ~: disasm' "864012" ~?= "xchg al,[bx+si+0x12]"
    , "86-87 mod=01,w=0 2" ~: disasm' "867F12" ~?= "xchg bh,[bx+0x12]"
    , "86-87 mod=10,w=0 1" ~: disasm' "86801234" ~?= "xchg al,[bx+si+0x3412]"
    , "86-87 mod=10,w=0 2" ~: disasm' "868FFFFF" ~?= "xchg cl,[bx-0x1]"
    , "86-87 mod=11,w=0 1" ~: disasm' "86C0" ~?= "xchg al,al"
    , "86-87 mod=11,w=0 2" ~: disasm' "86FF" ~?= "xchg bh,bh"
    , "86-87 mod=00,w=1 1" ~: disasm' "8700" ~?= "xchg ax,[bx+si]"
    , "86-87 mod=00,w=1 2" ~: disasm' "873F" ~?= "xchg di,[bx]"
    , "86-87 mod=01,w=1 1" ~: disasm' "874012" ~?= "xchg ax,[bx+si+0x12]"
    , "86-87 mod=01,w=1 2" ~: disasm' "877F12" ~?= "xchg di,[bx+0x12]"
    , "86-87 mod=10,w=1 1" ~: disasm' "87801234" ~?= "xchg ax,[bx+si+0x3412]"
    , "86-87 mod=10,w=1 2" ~: disasm' "87BFFFFF" ~?= "xchg di,[bx-0x1]"
    , "86-87 mod=11,w=0 1" ~: disasm' "87C0" ~?= "xchg ax,ax"
    , "86-87 mod=11,w=1 1" ~: disasm' "87FF" ~?= "xchg di,di"
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
    , "8d 1" ~: disasm' "8D00" ~?= "lea ax,[bx+si]"
    , "8d 2" ~: disasm' "8D4012" ~?= "lea ax,[bx+si+0x12]"
    , "8d 3" ~: disasm' "8D801234" ~?= "lea ax,[bx+si+0x3412]"
    , "8d 4" ~: disasm' "8DC0" ~?= "lea ax,ax"
    -- lds
    , "c5 1" ~: disasm' "C500" ~?= "lds ax,[bx+si]"
    , "c5 2" ~: disasm' "C54012" ~?= "lds ax,[bx+si+0x12]"
    , "c5 3" ~: disasm' "C5801234" ~?= "lds ax,[bx+si+0x3412]"
    , "c5 4" ~: disasm' "C5C0" ~?= "lds ax,ax"
    -- les
    , "c4 1" ~: disasm' "C400" ~?= "les ax,[bx+si]"
    , "c4 2" ~: disasm' "C44012" ~?= "les ax,[bx+si+0x12]"
    , "c4 3" ~: disasm' "C4801234" ~?= "les ax,[bx+si+0x3412]"
    , "c4 4" ~: disasm' "C4C0" ~?= "les ax,ax"
    -- lahf
    , "9f 1" ~: disasm' "9f" ~?= "lahf"
    -- sahf
    , "9e 1" ~: disasm' "9e" ~?= "sahf"
    -- pushf
    , "9c 1" ~: disasm' "9c" ~?= "pushfw"
    -- popf
    , "9d 1" ~: disasm' "9d" ~?= "popfw"
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
