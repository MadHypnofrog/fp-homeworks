import Test.Hspec

import Lib ( stringSum
           , Expr (..)
           , ArithmeticError (..)
           , eval
           , Parser (runParser)
           , ok
           , eof
           , satisfy
           , element
           , stream
           , parenthesesParser
           , numberParser
           , listlistParser
           )

main :: IO ()
main = hspec $ do spec

spec :: Spec
spec = describe "eval" $ do 
        it "Constant" $ 
          eval (Const 4) `shouldBe` Right 4
        it "Negative constant" $
          eval (Const (-34)) `shouldBe` Right (-34)
        it "a + b" $
          eval (Sum (Const 2) (Const 5)) `shouldBe` Right (2 + 5)
        it "a + (- b)" $
          eval (Sum (Const 2) (Const (-5))) `shouldBe` Right (2 + (-5))
        it "a - (- b)" $
          eval (Sub (Const 4) (Const 6)) `shouldBe` Right (4 - 6)
        it "a - (- b)" $
          eval (Sub (Const 4) (Const (-6))) `shouldBe` Right (4 - (-6))
        it "a * b" $
          eval (Mul (Const 3) (Const 5)) `shouldBe` Right (3 * 5)
        it "(- a) * b" $
          eval (Mul (Const (-3)) (Const 5)) `shouldBe` Right ((-3) * 5)
        it "a / b, b != 0" $
          eval (Div (Const 432) (Const 4)) `shouldBe` Right (432 `div` 4)
        it "a / b, b == 0" $
          eval (Div (Const 2) (Const 0)) `shouldBe` Left DivisionByZero
        it "a / b, b == 0 with evaluating" $
          eval (Div (Const 2) (Sub (Const 3) (Const 3))) `shouldBe` Left DivisionByZero
        it "0 / b, b != 0" $
          eval (Div (Const 0) (Const 5)) `shouldBe` Right (0 `div` 5)
        it "a ^ b, b > 0" $
          eval (Pow (Const 2) (Const 5)) `shouldBe` Right 32
        it "a ^ b, b < 0" $
          eval (Pow (Const 2) (Const (-5))) `shouldBe` Left NegativePower
        it "Random complex text" $
          eval (Div (Mul (Sum (Const 5) (Const (-1))) (Pow (Const 22) (Sub (Const 6) (Const 6)))) (Const 4)) `shouldBe` Right (((5 + (-1)) * (1)) `div` 4)

        describe "stringSum" $ do
          it "Constant" $
            stringSum "34" `shouldBe` Just 34
          it "Negative onstant" $
            stringSum "-34" `shouldBe` Just (-34)
          it "Whitespaces" $
            stringSum "123\t\n\t\n\t\n321 -4 -40" `shouldBe` Just (123 + 321 - 4 - 40)
          it "Whitespaces 2" $
            stringSum "\n1\t\n3   555  -1\n\n\n-5" `shouldBe` Just (1 + 3 + 555 - 1 - 5)
          it "Illegal characters" $
            stringSum "1 asd 1" `shouldBe` Nothing
          it "Illegal numbers" $
            stringSum "1-1" `shouldBe` Nothing
          it "Illegal numbers 2" $
            stringSum "--2" `shouldBe` Nothing
          it "Illegal numbers 3" $
            stringSum "+1" `shouldBe` Nothing

        describe "Parser combinators" $ do
          it "ok (empty string)" $
            runParser ok "" `shouldBe` Just ((), "")
          it "ok (string)" $
            runParser ok "value" `shouldBe` Just ((), "value") 
          it "ok (array)" $
            runParser ok [1 :: Int, 2] `shouldBe` Just ((), [1, 2])
          it "eof (empty string)" $
            runParser eof "" `shouldBe` Just ((), "")
          it "eof (string)" $
            runParser eof "value" `shouldBe` Nothing
          it "satisfy (elem)" $
            runParser (satisfy (< 5)) [0 :: Int, 1] `shouldBe` Just (0, [1])
          it "satisfy (incorrect elem)" $
            runParser (satisfy (< 5)) [10 :: Int, 1] `shouldBe` Nothing
          it "element (elem)" $
            runParser (element 0) [0 :: Int, 1] `shouldBe` Just (0, [1])
          it "element (incorrect elem)" $
            runParser (element 0) [10 :: Int, 1] `shouldBe` Nothing
          it "stream (prefix)" $
            runParser (stream [1, 2, 3]) [1 :: Int, 2, 3, 4, 5] `shouldBe` Just ([1, 2, 3], [4, 5])
          it "stream (incorrect prefix)" $
            runParser (stream "pref__") "prefix" `shouldBe` Nothing

        describe "parenthesesParser" $ do
          it "Empty string" $
            runParser parenthesesParser "" `shouldBe` Just ((), "")
          it "Correct sequence 1" $
            runParser parenthesesParser "()" `shouldBe` Just ((), "")
          it "Correct sequence 2" $
            runParser parenthesesParser "()()()()()" `shouldBe` Just ((), "")
          it "Correct sequence 3" $
            runParser parenthesesParser "((((()))))" `shouldBe` Just ((), "")
          it "Correct sequence 4" $
            runParser parenthesesParser "(())()()(()())((()()()))" `shouldBe` Just ((), "")
          it "Correct prefix but incorrect sequence" $
            runParser parenthesesParser "())" `shouldBe` Nothing
          it "Incorrect sequence 1" $
            runParser parenthesesParser ")" `shouldBe` Nothing
          it "Incorrect sequence 2" $
            runParser parenthesesParser "))))))))))" `shouldBe` Nothing
          it "Incorrect sequence 3" $
            runParser parenthesesParser "((((())))))" `shouldBe` Nothing
          it "Invalid characters" $
            runParser parenthesesParser "(((((a))))))" `shouldBe` Nothing

        describe "numberParser" $ do
          it "Empty string" $
            runParser numberParser "" `shouldBe` Nothing
          it "Constant" $
            runParser numberParser "23" `shouldBe` Just (23, "")
          it "Negative constant" $
            runParser numberParser "-23" `shouldBe` Just (-23, "")
          it "Unary plus" $
            runParser numberParser "+23" `shouldBe` Just (23, "")
          it "Invalid characters in the end 1" $
            runParser numberParser "23aasd" `shouldBe` Just (23, "aasd")
          it "Invalid characters in the end 2" $
            runParser numberParser "23 1" `shouldBe` Just (23, " 1")
          it "Invalid characters in the beginning 1" $
            runParser numberParser "as23" `shouldBe` Nothing
          it "Invalid characters in the beginning 2" $
            runParser numberParser " 23" `shouldBe` Nothing

        describe "listlistParser" $ do
          it "Empty string" $
            runParser listlistParser "" `shouldBe` Just ([], "")
          it "String of spaces" $
            runParser listlistParser "       " `shouldBe` Just ([], "")
          it "Empty list" $
            runParser listlistParser "0" `shouldBe` Just ([[]], "")
          it "Negative list size" $
            runParser listlistParser "-1, 1, 1" `shouldBe` Nothing
          it "Correct test 1" $
            runParser listlistParser "  3, -1, +22,    1  , 0  , 0" `shouldBe` Just ([[-1, 22, 1], [], []], "")
          it "Correct test 2" $
            runParser listlistParser " 6 , 1 , 1,1,1   , 1,  1 , 1 , -1" `shouldBe` Just ([[1, 1, 1, 1, 1, 1], [-1]], "")
          it "Extra comma in the end" $
            runParser listlistParser " 6 , 1 , 1,1,1   , 1,  1 , 1 , -1," `shouldBe` Nothing
          it "Extra comma in the middle" $
            runParser listlistParser " 6 , 1 , 1,1,1 ,  , 1,  1 , 1 , -1," `shouldBe` Nothing
          it "No comma between numbers" $
            runParser listlistParser " 6 , 1  1,1,1   , 1,  1 , 1 , -1," `shouldBe` Nothing
          it "Invalid characters in the end" $
            runParser listlistParser " 6 , 1 , 1,1,1   , 1,  1 , 1 , -1   asd" `shouldBe` Nothing
          it "Invalid characters" $
            runParser listlistParser " 6 , 1dd , 1,1,1   , 1,  1 , 1 , -1   asd" `shouldBe` Nothing
