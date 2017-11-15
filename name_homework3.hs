{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
module Homework3 where
import Control.Exception (evaluate)
import Test.Hspec
import RPNAST


prob1 :: String -> PExp
prob1 x = helper (words x)
	where
		helper (x:xs) 
			| x == "+" = Plus : helper xs
			| x == "-" = Minus : helper xs
			| x == "*" = Mul : helper xs
			| x == "/" = IntDiv : helper xs
			| otherwise = Val (read x :: Int) : helper xs
		helper [] = []
			
prob2 :: PExp -> Int
prob2 x = helper x []
	where
		helper (Val x: xs) vals       = helper xs (x:vals)
		helper (Plus:xs) (r:l:vals)   = helper xs ((l + r):vals)
		helper (Mul:xs) (r:l:vals)    = helper xs ((l * r):vals)
		helper (IntDiv:xs) (0:l:vals) = errorWithoutStackTrace "Cannot divide by zero!"
		helper (IntDiv:xs) (r:l:vals) = helper xs ((l `div` r):vals)
		helper (Minus:xs) (r:l:vals)  = helper xs ((l - r):vals)
		helper [] [i]                 = i
		helper _ _                    = errorWithoutStackTrace "Bad Input"


prob3 :: PExp -> RPNResult
prob3 x = helper x []
	where
		helper (Val x: xs) vals       = helper xs (x:vals)
		helper (Plus:xs) (r:l:vals)   = helper xs ((l + r):vals)
		helper (Mul:xs) (r:l:vals)    = helper xs ((l * r):vals)
		helper (IntDiv:xs) (0:l:vals) = Failure DivByZero
		helper (IntDiv:xs) (r:l:vals) = helper xs ((l `div` r):vals)
		helper (Minus:xs) (r:l:vals)  = helper xs ((l - r):vals)
		helper [] [i]                 = Success i
		helper _ _                    = Failure BadSyntax

prob4    :: a
prob4    = undefined

-- Write your Hspec Tests below
test_prob1 :: IO()
test_prob1 = hspec $ do 
	describe "Prob1 from HW3" $ do
		context "For \"200 + - * /\"" $ do
			it "should return [Val 200, Plus, Minus, Mul, IntDiv]" $ do
				prob1 "200 + - * /" `shouldBe` [Val 200, Plus, Minus, Mul, IntDiv]

		context "For \"+ - * / 200\"" $ do
			it "should return [Plus, Minus, Mul, IntDiv, Val 200]" $ do
				prob1 "+ - * / 200" `shouldBe` [Plus, Minus, Mul, IntDiv, Val 200]

		context "For \"4 + 5 * 2\"" $ do
			it "should return [Val 4, Plus, Val 5, Mul, Val 2]" $ do
				prob1 "4 + 5 * 2" `shouldBe` [Val 4, Plus, Val 5, Mul, Val 2]
test_prob2 :: IO()
test_prob2 = hspec $ do
	describe "Prob2 from HW3" $ do
		context "For [Val 4, Val 2, IntDiv]" $ do
			it "should return 2" $ do
				prob2 [Val 4, Val 2, IntDiv] `shouldBe` 2

		context "For [Mul]" $ do 
			it "should return \"Exception: Bad Input\"" $ do
				evaluate (prob2 [Mul]) `shouldThrow` anyException

		context "For [Val 4, Val 0, IntDiv]" $ do
			it "should return \"Cannot divide by zero!\"" $ do
				evaluate (prob2 [Val 4, Val 0, IntDiv]) `shouldThrow` anyException

test_prob3 :: IO()
test_prob3 = hspec $ do
	describe "Prob3 from HW3" $ do
		context "For [Val 5, Val 0, IntDiv]" $ do
			it "should return Failure DivByZero" $ do
				prob3 [Val 5, Val 0, IntDiv] `shouldBe` Failure DivByZero

		context "For [IntDiv, Plus, Val 0]" $ do
			it "should return Failure BadSyntax" $ do
				prob3 [IntDiv, Plus, Val 0] `shouldBe` Failure BadSyntax

		context "For [Val 5, Val 1, Val 1, Plus, Mul]" $ do 
			it "should return Success 10" $ do
				prob3 [Val 5, Val 1, Val 1, Plus, Mul] `shouldBe` Success 10







