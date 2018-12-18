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
            deriving (Eq)

    instance Show Expr where
        show = showExpr
      
    --Test expressions
    --ex3 = Mul (Add (Num 1) (Num 2)) (Num 3)
    --ex2 = Add (Num 1) (Mul (Num 2) (Num 3))  -- 1+2*3

    -----------------------------------------------
    -- B   

    -- | Converts any expression to a string.
    showExpr :: Expr -> String
    showExpr (Num n)     = show n
    showExpr (Var c)     = "x"
    showExpr (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
    showExpr (Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2
    showExpr (Sin e)     = "sin(" ++ showExpr e ++ ")"
    showExpr (Cos e)     = "cos(" ++ showExpr e ++ ")"
    
    showFactor :: Expr -> String
    showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
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
    -- | which simplifies expressions so that subexpressions not involving variables  
    -- | are always simplified to their smallest representation
    simplify :: Expr -> Expr
    simplify s | (simplify' s) == s = s
               | otherwise = simplify' (simplify' s)
     

    simplify' :: Expr -> Expr
    -- base cases
    simplify' (Num s') = Num s'
    simplify' (Var v') = Var 'x'
    
    -- n * 0 = 0
    simplify' (Mul (Num 0.0) s') = Num 0
    simplify' (Mul s' (Num 0.0)) = Num 0
    
    -- x+0 = x
    simplify' (Add (Num 0.0) (Var x)) = Var x
    simplify' (Add (Var x) (Num 0.0) ) = Var x

    -- expr + 0 = expr
    simplify' (Add s (Num 0.0) ) = simplify' s
    simplify' (Add (Num 0.0) s ) = simplify' s

    -- x*1 = x
    simplify' (Mul (Num 1.0) (Var x)) = Var x
    simplify' (Mul (Var x) (Num 1.0)) = Var x

    -- n*1 = n
    simplify' (Mul (Num 1.0) s') = simplify s'
    simplify' (Mul s' (Num 1.0)) = simplify s'

    -- n+m & expr + expr
    simplify' (Add (Num n) (Num m)) = Num (n+m)
    simplify' (Add e1 e2) = (Add (simplify e1) (simplify e2))

    --expr * n & n*m
    simplify' (Mul (Num n) (Num m)) = Num (n*m)
    simplify' (Mul s' (Num n)) = (Mul (Num n) (simplify s'))
    simplify' (Mul (Num n) s') = (Mul (simplify s') (Num n))
    simplify' (Mul e1 e2) = (Mul (simplify e1) (simplify e2))

    simplify' (Sin s') = (Sin (simplify s'))
    simplify' (Cos s') = (Cos (simplify s'))   
    
    prop_Simplify :: Expr -> Bool
    prop_Simplify s = checkSimplified (simplify s)

    checkSimplified :: Expr -> Bool
    checkSimplified (Var s) = True
    checkSimplified (Num n) = if n == 0.0 then False else True
    checkSimplified (Mul  _ (Num 1.0)) = False
    checkSimplified (Mul (Num 1.0) _ ) = False
    checkSimplified (Sin (Num 0.0)) = True
    checkSimplified (Cos (Num 0.0)) = True
    checkSimplified (Add s1 s2) = checkSimplified s1 && checkSimplified s2
    checkSimplified (Mul s1 s2) = checkSimplified s1 && checkSimplified s2
    checkSimplified (Sin (s1)) = checkSimplified s1 
    checkSimplified (Cos (s1)) = checkSimplified s1 
    
    -----------------------------------------------
    -- G
    
    differentiate :: Expr -> Expr
    differentiate e = simplify (differentiate' e)
      
    differentiate' :: Expr -> Expr
    differentiate' (Num n)     = Num 0.0
    differentiate' (Var x)     = Num 1.0
    differentiate' (Add e1 e2) = (Add (differentiate' e1) (differentiate' e2))
    differentiate' (Mul e1 e2) = (Add (Mul e1 (differentiate' e2)) (Mul (differentiate' e1) e2))
    differentiate' (Sin e)     = (Cos e)
    differentiate' (Cos e)     = (Mul (Num (-1.0)) (Sin e))
