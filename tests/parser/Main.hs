{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import CrispVal ( CrispVal(String, Nil, Atom, List, Bool, Number) )
import Parser ( readCode )

import qualified Data.Text as T 
import Data.Text (Text)

import Test.Hspec (hspec, describe, it, shouldBe)

main :: IO ()
main = do 
    hspec $ describe "src/Parser.hs" $ do 

        it "Atom" $
            readCode "abc_25b"  `shouldBe` Right (Atom "abc_25b")
        
        it "Negative Num" $
            readCode "-2525"  `shouldBe` Right (Number (-2525))
        
        it "Positive Num" $
            readCode "25"  `shouldBe` Right (Number (25))
        
        it "Positive Num with sign" $
            readCode "+25" `shouldBe` Right (Number (25))

        it "String" $
            readCode "\"This is a string\"" `shouldBe` 
                Right (String "This is a string")

        it "Bool True" $
            readCode "True" `shouldBe` Right (Bool True)

        it "Bool False" $
            readCode "False" `shouldBe` Right (Bool False)

        it "Nil" $
            readCode "'()" `shouldBe` Right Nil

        it "S-expr homolist 1" $
            readCode "(1 2 4)" `shouldBe` 
                Right (List [Number 1, Number 2, Number 4])
        
        it "S-expr homolist 2" $
            readCode "(+1 2 -4)" `shouldBe` 
                Right (List [Number 1, Number 2, Number (-4)])
        
        it "S-expr homolist 3" $
            readCode "(\"a\" \"b\" \"chia+\")" `shouldBe` 
                Right (List [String "a", String "b", String "chia+"])

        it "S-expr homolist quoted" $
            readCode "'(\"a\" \"b\" \"chia+\")" `shouldBe` 
                Right (List [Atom "quote", List [String "a", String "b", String "chia+"]])

        it "S-expr heterolist 1" $
            readCode "(1 2 4)" `shouldBe` 
                Right (List [Number 1, Number 2, Number 4])
        
        