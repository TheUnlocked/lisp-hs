{-# LANGUAGE LambdaCase #-}

module ParserSpec (readLispSpec) where

import Test.Hspec
import Test.QuickCheck

import GHC.Real
import Parser

shouldJustBe :: (Show a, Show b, Eq a, Eq b) => Either a [b] -> b -> Expectation
shouldJustBe x y = x `shouldBe` Right [y]
shouldFailWith :: (Show a, Show b, Eq a) => Either [a] [b] -> a -> Expectation
shouldFailWith x y = x `shouldSatisfy` \case
    Left err -> y `elem` err
    Right _  -> False

readLispSpec :: SpecWith ()
readLispSpec = describe "readLisp" $ do
    it "succeeds on empty string" $ do
        readLisp "" `shouldBe` Right []

    describe "whitespace" $ do
        it "ignores leading whitespace" $ do
            property $ \(Positive (Small x)) ->
                readLisp (replicate x ' ' ++ "a") `shouldJustBe` LispSymbol "a"
        it "ignores trailing whitespace" $ do
            property $ \(Positive (Small x)) ->
                readLisp ("a" ++ replicate x ' ') `shouldJustBe` LispSymbol "a"
        it "ignores leading and trailing whitespace" $ do
            property $ \(Positive (Small x)) (Positive (Small y)) ->
                readLisp (replicate x ' ' ++ "a" ++ replicate y ' ') `shouldJustBe` LispSymbol "a"

    describe "comments" $ do
        it "can parse \";abc<newline>1\" (excl. quotes) as 1" $ do
            readLisp ";abc\n1" `shouldJustBe` LispRational (1 % 1)
        it "can parse \"(a b ;test<newline> c)\" (excl. quotes) as (a b c)" $ do
            readLisp "(a b ;test\n c)" `shouldJustBe` toLispList [LispSymbol "a", LispSymbol "b", LispSymbol "c"]
        it "can parse \"Hello, ;<newline>World!\" as \"Hello, ;\\nWorld!\"" $ do
            readLisp "\"Hello, ;\nWorld!\"" `shouldJustBe` LispString "Hello, ;\nWorld!"
        it "ignores trailing comments" $ do
            readLisp "a ;test" `shouldJustBe` LispSymbol "a"

    describe "integers" $ do
        it "can parse random non-negative integers" $ do
            property $ \(NonNegative x) ->
                readLisp (show x) `shouldJustBe` LispRational (x % 1)
        it "can parse random negative integers" $ do
            property $ \(Positive x) ->
                readLisp ('-' : show x) `shouldJustBe` LispRational (-x % 1)
        it "can parse random non-negative integers with leading zeroes" $ do
            property $ \(NonNegative x) (Small y) ->
                readLisp (replicate y '0' ++ show x) `shouldJustBe` LispRational (x % 1)
        it "can parse random negative integers with leading zeroes" $ do
            property $ \(Positive x) (Small y) ->
                readLisp ('-' : replicate y '0' ++ show x) `shouldJustBe` LispRational (-x % 1)

    describe "rationals" $ do
        it "fails to parse random fractions with a zero denominator" $ do
            property $ \x ->
                readLisp (show x ++ "/0") `shouldFailWith` DivideByZeroFraction x
        it "can parse random fractions" $ do
            property $ \x (NonZero y) ->
                readLisp (show x ++ '/' : show y) `shouldJustBe` LispRational (x % y)

    describe "doubles" $ do
        it "can parse 0.0" $ do
            readLisp "0.0" `shouldJustBe` LispDouble 0
        it "can parse 1.0" $ do
            readLisp "1.0" `shouldJustBe` LispDouble 1
        it "can parse -1.2" $ do
            readLisp "-1.2" `shouldJustBe` LispDouble (-1.2)
        it "can parse random doubles" $ do
            property $ \x ->
                readLisp (show x) `shouldJustBe` LispDouble x

    describe "symbols" $ do
        it "can parse abc" $ do
            readLisp "abc" `shouldJustBe` LispSymbol "abc"
        it "can parse +" $ do
            readLisp "+" `shouldJustBe` LispSymbol "+"

    describe "lists" $ do
        it "can parse the empty list" $ do
            readLisp "()" `shouldJustBe` LispNil
        it "can parse ( )" $ do
            readLisp "( )" `shouldJustBe` LispNil
        it "can parse (+ 1 2)" $ do
            readLisp "(+ 1 2)" `shouldJustBe` toLispList [LispSymbol "+", LispRational (1 % 1), LispRational (2 % 1)]
        it "can parse ( a b c )" $ do
            readLisp "( a b c )" `shouldJustBe` toLispList [LispSymbol "a", LispSymbol "b", LispSymbol "c"]
        it "can parse ((a) b)" $ do
            readLisp "((a) b)" `shouldJustBe` toLispList [toLispList [LispSymbol "a"], LispSymbol "b"]
        it "can parse (a (b))" $ do
            readLisp "(a (b))" `shouldJustBe` toLispList [LispSymbol "a", toLispList [LispSymbol "b"]]
        it "fails to parse a missing right paren" $ do
            readLisp "(a b c" `shouldFailWith` MissingClosingParen
        it "fails to parse a missing left paren" $ do
            readLisp "a b c)" `shouldFailWith` ExtraClosingParen
        it "can parse (a . b)" $ do
            readLisp "(a . b)" `shouldJustBe` LispPair (LispSymbol "a") (LispSymbol "b")
        it "can parse (a b . c)" $ do
            readLisp "(a b . c)" `shouldJustBe` LispPair (LispSymbol "a") (LispPair (LispSymbol "b") (LispSymbol "c"))
        it "fails to parse (a b . c .)" $ do
            readLisp "(a b . c .)" `shouldFailWith` InvalidPairDot
        it "fails to parse (a b .)" $ do
            readLisp "(a b .)" `shouldFailWith` InvalidPairDot
        it "fails to parse (a b ." $ do
            readLisp "(a b ." `shouldFailWith` InvalidPairDot

    describe "quotes" $ do
        it "can parse 'abc" $ do
            readLisp "'abc" `shouldJustBe` toLispList [LispSymbol "quote", LispSymbol "abc"]
        it "can parse '5" $ do
            readLisp "'5" `shouldJustBe` toLispList [LispSymbol "quote", LispRational (5 % 1)]
        it "can parse '()" $ do
            readLisp "'()" `shouldJustBe` toLispList [LispSymbol "quote", toLispList []]
        it "can parse '(a b c)" $ do
            readLisp "'(a b c)" `shouldJustBe` toLispList [LispSymbol "quote", toLispList [LispSymbol "a", LispSymbol "b", LispSymbol "c"]]
        it "can parse ''abc" $ do
            readLisp "''abc" `shouldJustBe` toLispList [LispSymbol "quote", toLispList [LispSymbol "quote", LispSymbol "abc"]]
        it "fails to parse a single quote" $ do
            readLisp "'" `shouldFailWith` StrandedQuote
    
    describe "strings" $ do
        it "can parse \"Hello, World!\"" $ do
            readLisp "\"Hello, World!\"" `shouldJustBe` LispString "Hello, World!"
        it "can parse \"\\\"Hello, World!\\\"\"" $ do
            readLisp "\"\\\"Hello, World!\\\"\"" `shouldJustBe` LispString "\"Hello, World!\""
        it "can parse \"\\n\"" $ do
            readLisp "\"\\n\"" `shouldJustBe` LispString "\n"
        it "can parse an escaped newline as a newline" $ do
            readLisp "\"\\\n\"" `shouldJustBe` LispString "\n"
        it "can parse \"\\\\\"" $ do
            readLisp "\"\\\\\"" `shouldJustBe` LispString "\\"
        it "can parse \"\\x41pple\"" $ do
            readLisp "\"\\x41pple\"" `shouldJustBe` LispString "Apple"