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

        it "S-expr: homolist 1" $
            readCode "(1 2 4)" `shouldBe` 
                Right (List [Number 1, Number 2, Number 4])
        
        it "S-expr: homolist 2" $
            readCode "(+1 2 -4)" `shouldBe` 
                Right (List [Number 1, Number 2, Number (-4)])
        
        it "S-expr: homolist 3" $
            readCode "(\"a\" \"b\" \"chia+\")" `shouldBe` 
                Right (List [String "a", String "b", String "chia+"])

        it "S-expr: homolist quoted" $
            readCode "'(\"a\" \"b\" \"chia+\")" `shouldBe` 
                Right (List [Atom "quote", List [String "a", String "b", String "chia+"]])

        it "S-expr: heterolist 1" $
            readCode "(1 2 haha \"str\")" `shouldBe` 
                Right (List [Number 1, Number 2, Atom "haha", String "str"])
        
        it "S-expr: single integer" $ 
            readCode "(-42)"  `shouldBe` Right (List [Number (-42)])

        it "S-expr: (- n)" $ 
            readCode "(- 42)"  `shouldBe` Right (List [Atom "-", Number (42)])

        it "S-expr: operator call" $ 
            readCode "(- -42 +42)"  `shouldBe` Right (List [Atom "-", Number (-42), Number 42])

        it "S-expr: operator call with atoms" $ 
            readCode "(- me you)"  `shouldBe` Right (List [Atom "-", Atom "me", Atom "you"])

        it "S-expr: nested list 1" $ 
            readCode "(let (x 20) (+ y x))"  `shouldBe` 
                Right (List [Atom "let", List [Atom "x", Number 20], List [Atom "+", Atom "y", Atom "x"]])
