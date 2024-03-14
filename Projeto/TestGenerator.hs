module TestGenerator where

import Test.QuickCheck

import PicoC


-- Type Generator
genType :: Gen Type
genType = elements [Int, Char, String, Bool, Void]

genInt :: Gen Int
genInt = choose (0, 100)

genChar :: Gen Char
genChar = elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ', '\n', '\t'])

genString :: Gen String
genString = listOf genChar -- Put a size limit?????????

genBool :: Gen Bool
genBool = choose (False, True)



-- 