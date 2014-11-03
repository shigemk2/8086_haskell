module Main where

import Test.HUnit
import System.IO
import Data.Char
import Data.Bits

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

-- メモリをシミュレートするためバイト区切りのリスト
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

-- バイトオーダー
-- メモリはバイトごとに区切られています。1バイトに収まりきらない数値は分割して格納します
-- 分割した際に逆順に並べ替える方式をリトルエンディアンと呼びます。並べ替えない方式はビッグエンディアン
-- 8086以降はリトルエンディアン
-- バイトごとに手で区切る
-- バイトオーダーを考えるのは分割後です。分割前のバイトオーダーは考えません
-- 数値→リトルエンディアン
toLE 0 _ = []
toLE n x = x `mod` 0x100 : toLE (n - 1) (x `div` 0x100)
-- リトルエンディアン→数値
fromLE 0 _      = 0
fromLE n (x:xs) = x + 0x100 * fromLE (n - 1) xs
-- 数値→ビッグエンディアン
-- 計算がリトルエンディアンより計算が難しい
toBE 0 _ = []
toBE n x = x `div` (0x100 ^ (n - 1)) `mod` 0x100 : toBE (n - 1) x
-- ビッグエンディアン→数値
fromBE 0 _ = 0
fromBE n (x:xs) = x * 0x100^(n - 1) + fromBE (n - 1) xs

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
        -- 桁数制限ではみ出した奴は消える
        , "toLE 2" ~: toLE 2 0x10000    ~?= [0, 0]
        , "toLE 3" ~: toLE 4 0x12345678 ~?= [0x78, 0x56, 0x34, 0x12]
        , "fromLE 1" ~: fromLE 2 [0, 1]                   ~?= 0x100
        , "fromLE 2" ~: fromLE 2 [0x78, 0x56, 0x34, 0x12] ~?= 0x5678
        , "fromLE 3" ~: fromLE 4 [0x78, 0x56, 0x34, 0x12] ~?= 0x12345678
        , "toBE 1" ~: toBE 2 1          ~?= [0, 1]
        , "toBE 2" ~: toBE 2 0x10000    ~?= [0, 0]
        , "toBE 3" ~: toBE 4 0x12345678 ~?= [0x12, 0x34, 0x56, 0x78]
        , "fromBE 1" ~: fromBE 2 [0, 1]                   ~?= 0x1
        , "fromBE 2" ~: fromBE 2 [0x78, 0x56, 0x34, 0x12] ~?= 0x7856
        , "fromBE 3" ~: fromBE 4 [0x78, 0x56, 0x34, 0x12] ~?= 0x78563412
        ]


-- 逆アセンブル結果
-- アセンブリをみてマシン語をみて、なんとなくわかればいいんじゃなかろうか
-- B83412            mov ax,0x1234
-- B83412を機械語、movをニーモニック、axや0x1234をオペランドと呼びます。
-- 機械語はマシン語ともいう
-- 命令の分量はi7の1/5

-- disasm (x:xs)
--     | x == 0xb8 =
--         "mov ax,0x" ++ hex (fromLE 2 xs)
-- とりあえずmov命令あたりから初めて行って、オペコードの場合をすべて網羅していく
-- すべての命令を網羅するのが10とすると、今日の入門編でやるのは2
-- disasm (x:xs)
--     | 0xb0 <= x && x <= 0xb7 =
--         "mov " ++ reg8  !! (x - 0xb0) ++ ",0x" ++ hex (xs !! 0)
--     | 0xb8 <= x && x <= 0xbf =
--         "mov " ++ reg16 !! (x - 0xb8) ++ ",0x" ++ hex (fromLE 2 xs)
regs  = [reg8, reg16]

-- Haskellは2進数を直接かけない
-- 即値(ハードコードされた数値)
disasm (x:xs)
    -- DATA TRANSFER
    -- MOV = Move:
    -- Immediate to Register [1011wreg][data][data if w=1]
    | 0xb0 <= x && x <= 0xbf =
        "mov " ++ reg ++ "," ++ imm
        where
            w = (x `shiftR` 3) .&. 1
            reg = regs !! w !! (x .&. 7)
            imm = "0x" ++ hex (fromLE (w + 1) xs)

-- べんりかんすう
disasm' hex = disasm $ hexStrToList hex

-- レジスタ=固定の変数のようなもの
reg16 = ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di"]
-- 機械語のなかの一番重要な部分がオペコード
reg8  = ["al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"]

-- >>> bin(0xb0)
-- '0b10110000'
-- >>> bin(0xb8)
-- '0b10111000'
-- >>> bin(0xb9)
-- '0b10111001'
-- >>> bin(0xbf)
-- '0b10111111'
-- w=1がword(2バイト)命令 w=0がbyte(1バイト)命令
-- wの値によって、入れられるdata部分のサイズが8ビットか16ビットか分かるようになっている
-- 割り算は遅い処理なので、ビット演算でやる

-- ここまでの実装では16進数に変換して範囲チェックしていました。2進数のままパターンマッチできるように修正
-- byteは1バイトだけど、wordはCPUごとにバイト数が違う。8086だとwordは2バイトだが、ARMのwordは4バイト(wordは普遍じゃない)

-- 1バイトを8ビットに分解する関数(可変なのでtuppleで実装する)
getBits x = (b 7, b 6, b 5, b 4, b 3, b 2, b 1, b 0)
    where
        b n = (x `shiftR` n) .&. 1

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
    ]

main = do
    runTestText (putTextToHandle stderr False) (TestList [testHex, testDisAsm])
