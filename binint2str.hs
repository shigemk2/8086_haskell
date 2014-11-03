module Main where

import Test.HUnit
import System.IO

binIntToStr bin = ""
tests = TestList
    [ "binIntToStr 1" ~: binIntToStr 5 ~?= "101"
    , "binIntToStr 2" ~: binIntToStr 25 ~?= "11001"
    , "binIntToStr 3" ~: binIntToStr 31 ~?= "11111"
    , "binIntToStr 3" ~: binIntToStr 50 ~?= "110010"
    ]

main = do
    runTestText (putTextToHandle stderr False) tests
