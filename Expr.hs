module Expr where

import Data.Char
import Parsing
import Test.QuickCheck
import Data.Maybe


data Expr = Num Double
          | Add Expr Expr
          | Mul Expr Expr
          | Var Char
          | Sin Expr
          | Cos Expr
        deriving (Show, Eq)


-----------------------------------------------
-- B som i bäst

-- | Showing expressions
showExpr :: Expr -> String
showExpr (Num n)     = show n
showExpr (Var c)     = "x"
showExpr (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2
showExpr (Sin e)     = "sin(" ++ showExpr e ++ ")"
showExpr (Cos e)     = "cos(" ++ showExpr e ++ ")"

showFactor :: Expr -> String
showFactor (Add e1 e2)= "(" ++ showExpr (Add e1 e2) ++ ")"
showFactor e           = showExpr e

-----------------------------------------------
-- C

-- | Given an expression, and the value for the variable x, calculates the value of the expression.
eval :: Expr -> Double -> Double
eval (Num n) x     = n
eval (Var c) x     = x
eval (Add e1 e2) x = (eval e1 x) + (eval e2 x)
eval (Mul e1 e2) x = (eval e1 x) * (eval e2 x)
eval (Sin e1) x    = sin (eval e1 x)
eval (Cos e1) x    = cos (eval e1 x)

-----------------------------------------------
-- D
-- Main goal, to define a top level parser

-- Function readExpr: Given a string, tries to interpret the string as an
-- expression, and returns Just of that expression if it succeeds. Otherwise,
-- Nothing will be returned.
readExpr :: String -> Maybe Expr
readExpr s = let s' = filter (not.isSpace) s
             in case parse expr s' of
                Just (e,"") -> Just e
                _           -> Nothing

num :: Parser Expr
num = Num <$> readsP

expr = foldr1 Add <$> chain term (char '+')

term = foldr1 Mul <$> chain factor (char '*')

sinFunc = Sin <$> ((char 's') *> (char 'i') *> (char 'n') *> factor) 

cosFunc = Cos <$> ((char 'c') *> (char 'o') *> (char 's') *> factor) 

var = Var <$> (char 'x')

factor = char '(' *> expr <* char ')' <|> num <|> 
         sinFunc <|> cosFunc <|> var

-----------------------------------------------
-- E
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = let s = showExpr e
                          Just e' = readExpr s
                      in showExpr e' == s

arbExpr :: Int -> Gen Expr
arbExpr s = frequency [(1, rNum), (1, rVar), (s, rOp s), (s, rFunc s)]
    where
        rNum = elements $ map Num [0.0..100.0]
        rVar = elements $ map Var ['x'] 
        rOp s = do
            let s' = (div s 2)
            op <- elements [Mul, Add]
            e1 <- arbExpr s'
            e2 <- arbExpr s'
            return $ op e1 e2
        rFunc s = do
            let s' = (div s 2)
            func <- elements [Sin, Cos]
            e <- arbExpr s'
            return $ func e

instance Arbitrary Expr where
    arbitrary = sized arbExpr

-----------------------------------------------
-- F

simplify :: Expr -> Expr
simplify e = do 
              let e' = simplify' e
              let result = if e' == e then e' else simplify e'
              result


simplify' :: Expr -> Expr
simplify' e = case e of 

        -- base cases
        (Num e')                             -> Num e'
        (Var 'x')                            -> Var 'x'

        --adding 0
        (Add (Num 0.0) e')                   -> simplify e'
        (Add e' (Num 0.0))                   -> simplify e'

        --multiplying by 0
        (Mul (Num 0.0) e')                   -> Num 0
        (Mul e' (Num 0.0))                   -> Num 0

        --multiplying by 1
        (Mul (Num 1.0) e')                   -> simplify e'
        (Mul e' (Num 1.0))                   -> simplify e'

        --simplifying additions
        (Add (Num n) (Num m))                -> Num (n+m)
        (Add e1 e2) | e1 == e2               -> (Mul (Num 2) (simplify e1))
        (Add (Mul (Num n) e1) e2) | e1 == e2 -> (Mul (Num (n+1)) (simplify e1))
        (Add e1 (Mul (Num n) e2)) | e1 == e2 -> (Mul (Num (n+1)) (simplify e1))
        (Add e1 e2)                          -> (Add (simplify e1) (simplify e2))

        --simplifying multiplications
        (Mul (Num n) (Num m))                -> Num (n*m)
        (Mul (Var 'x') (Var 'x'))            -> (Mul (Var 'x') (Var 'x'))
        (Mul e' (Num n))                     -> (Mul (Num n) (simplify e'))
        (Mul (Num n) (Var 'x'))              -> (Mul (Num n) (Var 'x'))
        (Mul e' (Var 'x'))                   -> (Mul (Var 'x') (simplify e'))
        (Mul (Num n) (Mul e' (Num m)))       -> (Mul (Num (n*m)) (simplify e'))
        (Mul (Num n) (Mul (Num m) e'))       -> (Mul (Num (n*m)) (simplify e'))
        (Mul (Var 'x') (Mul e' (Num n)))     -> (Mul (Num n) (Mul (simplify e') (Var 'x')))
        (Mul (Var 'x') (Mul (Num n) e'))     -> (Mul (Num n) (Mul (simplify e') (Var 'x')))
        (Mul e1 e2)                          -> (Mul (simplify e1) (simplify e2))

        --simplifying functions
        (Sin e')                             -> (Sin (simplify e'))
        (Cos e')                             -> (Cos (simplify e'))    

prop_Simplify :: Expr -> Double -> Bool
prop_Simplify e x=   (abs ((eval e x) - (eval (simplify e) x))) < 0.0001 

-----------------------------------------------
-- G

differentiate :: Expr -> Expr
differentiate e = simplify (differentiate' e)
  
differentiate' :: Expr -> Expr
differentiate' e = case e of
      (Num n)           -> Num 0.0
      (Var x)           -> Num 1.0
      (Add e1 e2)       -> (Add (differentiate' e1) (differentiate' e2))
      (Mul e1 e2)       -> (Add (Mul e1 (differentiate' e2)) 
                              (Mul (differentiate' e1) e2))
      (Sin e)           -> (Cos e)
      (Cos e)           -> (Mul (Num (-1.0)) (Sin e))