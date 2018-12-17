module Expr where

import Data.Char
import Parsing
import Test.QuickCheck
import Data.Maybe

-----------------------------------------------
-- A

-- | A recursive data typ representing mathematical expressions 
data Expr = Num Double
          | Add Expr Expr
          | Mul Expr Expr
          | Var Char
          | Sin Expr
          | Cos Expr
        deriving (Show, Eq)


-----------------------------------------------
-- B 
-- show returns a String. ( Show a => a -> String )

-- | Converts any expression to a string.
showExpr :: Expr -> String
showExpr (Num n)     = show n
showExpr (Var c)     = "x"
showExpr (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2
showExpr (Sin e)     = "sin(" ++ showExpr e ++ ")"
showExpr (Cos e)     = "cos(" ++ showExpr e ++ ")"

-- om det är mer än ett tal returneras det som en parantes, annars bara själv?? 
-- till mult i showExpr, så det blir rätt med paranteser tror jag.. 
-- Så att det som är i parentesen räkas ut först?
-- Nä jag tror bara det är för det visuella..? Den returnerar ju bara än sträng. fast ja sen ska det ju parsas och räknas rätt så då behövs väl paranteserna också ...'
-- Jag gissar här bara så du vet. Jag gillar det. <3 :')

showFactor :: Expr -> String
showFactor (Add e1 e2)= "(" ++ showExpr (Add e1 e2) ++ ")"
showFactor e          = showExpr e

-----------------------------------------------
-- C

-- | Given an expression, and the value for the variable x, calculates the value of the expression.
eval :: Expr -> Double -> Double
eval (Num n) x     = n
eval (Var c) x     = x
eval (Add e1 e2) x = (eval e1 x) + (eval e2 x)
eval (Mul e1 e2) x = (eval e1 x) * (eval e2 x) --Är detta ens matematiskt korrekt..? Tänker typ också de e fel. vad händer om x är 4? Den förvinner? Nä men jag tänker att den multipliceras för många gånger. först med e1 sen e2 sen multipliceras de ihop, räcker det inte med e1*e2*x. Men jag vet inte riktigt vad som händer så kanske blir så ändå
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
                Just (e,"") -> Just e --Var fan kommer e ifrån?
                _           -> Nothing

num :: Parser Expr
num = Num <$> readsP   -- (<$>) :: Functor f => (a -> b) -> f a -> f b
expr = foldr1 Add <$> chain term (char '+')
term = foldr1 Mul <$> chain factor (char '*')
sinFunc = Sin <$> ((char 's') *> (char 'i') *> (char 'n') *> factor) 
cosFunc = Cos <$> ((char 'c') *> (char 'o') *> (char 's') *> factor) 
var = Var <$> (char 'x') 
factor = char '(' *> expr <* char ')' <|> num <|> 
         sinFunc <|> cosFunc <|> var

-- http://www.cse.chalmers.se/edu/course/TDA452_Functional_Programming/lectures/Parsing.html
--Funktionerna ovan är tagna från föreläsningen. Page 26 och 27 och detta är det "snyggare sättet att skriva parsing".


-----------------------------------------------
-- E

--Test to see if the readExpr and showExpr returns the same result.
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = let s = showExpr e
                          Just e' = readExpr s
                      in showExpr e' == s

--generator for expressions
arbExpr :: Int -> Gen Expr
arbExpr i = frequency [(1, num), (1, var), (i, ope i), (i, func i)]
    where
        num = elements $ map Num [0.0..100.0]
        var = elements $ map Var ['x'] 
        ope i = do
            let i' = (div i 2)
            op <- elements [Mul, Add]
            e1 <- arbExpr i'
            e2 <- arbExpr i'
            return $ op e1 e2
        func i = do
            let i' = (div i 2)
            func <- elements [Sin, Cos]
            e <- arbExpr i'
            return $ func e

instance Arbitrary Expr where
    arbitrary = sized arbExpr

-----------------------------------------------
-- F
-- | which simplifies expressions so that subexpressions not involving variables are always simplified to their smallest representation
simplify :: Expr -> Expr
simplify s = do 
              let s' = simplify' s
              let result = if s' == s then s' else simplify s'
              result --vad är detta??


simplify' :: Expr -> Expr
simplify' s = case s of 

        -- base cases
        (Num s')                             -> Num s'
        (Var 'x')                            -> Var 'x'

        --adding 0
        (Add (Num 0.0) s')                   -> simplify s'
        (Add s' (Num 0.0))                   -> simplify s'

        --multiplying by 0
        (Mul (Num 0.0) s')                   -> Num 0
        (Mul s' (Num 0.0))                   -> Num 0

        --multiplying by 1
        (Mul (Num 1.0) s')                   -> simplify s'
        (Mul s' (Num 1.0))                   -> simplify s'

        --simplifying additions
        (Add (Num n) (Num m))                -> Num (n+m)
        (Add e1 e2) | e1 == e2               -> (Mul (Num 2) (simplify e1))
        (Add (Mul (Num n) e1) e2) | e1 == e2 -> (Mul (Num (n+1)) (simplify e1))
        (Add e1 (Mul (Num n) e2)) | e1 == e2 -> (Mul (Num (n+1)) (simplify e1))
        (Add e1 e2)                          -> (Add (simplify e1) (simplify e2))

        --simplifying multiplications
        (Mul (Num n) (Num m))                -> Num (n*m)
        (Mul (Var 'x') (Var 'x'))            -> (Mul (Var 'x') (Var 'x'))
        (Mul s' (Num n))                     -> (Mul (Num n) (simplify s'))
        (Mul (Num n) (Var 'x'))              -> (Mul (Num n) (Var 'x'))
        (Mul s' (Var 'x'))                   -> (Mul (Var 'x') (simplify s'))
        (Mul (Num n) (Mul s' (Num m)))       -> (Mul (Num (n*m)) (simplify s'))
        (Mul (Num n) (Mul (Num m) s'))       -> (Mul (Num (n*m)) (simplify s'))
        (Mul (Var 'x') (Mul s' (Num n)))     -> (Mul (Num n) (Mul (simplify s') (Var 'x')))
        (Mul (Var 'x') (Mul (Num n) s'))     -> (Mul (Num n) (Mul (simplify s') (Var 'x')))
        (Mul e1 e2)                          -> (Mul (simplify e1) (simplify e2))

        --simplifying functions
        (Sin s')                             -> (Sin (simplify s'))
        (Cos s')                             -> (Cos (simplify s'))    

prop_Simplify :: Expr -> Double -> Bool
prop_Simplify e x = (abs ((eval e x) - (eval (simplify e) x))) < 0.0001 

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