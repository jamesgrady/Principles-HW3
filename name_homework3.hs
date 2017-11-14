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
			
prob2    :: PExp -> Int
prob2 x = helper x []
	where
		helper (Val x : xs) stack = helper xs (stack ++ [x])
		helper _ [] = errorWithoutStackTrace "Bad Input"
		helper (Plus : xs) stack = helper xs ([solverPlus stack])
		helper (Mul : xs) stack = helper xs ([solverMul stack])
		helper (Minus : xs) stack = helper xs ([solverMinus stack])
		helper (IntDiv : xs) stack = helper xs ([solverIntDiv stack])
		helper [] stack = solverEnd stack
		helper _ _ = errorWithoutStackTrace "Bad Input"
		solverPlus [x , y] = x + y 
		solverMul [x , y] = x * y
		solverIntDiv [x , y] = if y == 0 then errorWithoutStackTrace "Cannot divide by zero" else  x `div` y
		solverMinus [x , y] = x - y
		solverEnd (x:xs) = x

prob3    :: a
prob3    = undefined

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
				prob2 [Mul] `shouldBe` errorWithoutStackTrace "Bad Input"

		context "For [Val 4, Val 0, IntDiv]" $ do
			it "should return \"Cannot divide by zero!\"" $ do
				prob2 [Val 4, Val 0, IntDiv] `shouldBe` errorWithoutStackTrace "Cannot divide by zero"









