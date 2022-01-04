module Expr where
import Parsing
import Data.Char
import Data.Maybe
import Test.QuickCheck
data Expr
    = Num Double
    | Add Expr Expr
    | Mul Expr Expr
    | Sin Expr
    | Cos Expr
    | X
    deriving (Show, Ord, Eq)

num :: Double -> Expr
num n = Num n

x :: Expr
x = X

add :: Expr -> Expr -> Expr
add exp1 exp2 = Add exp1 exp2

mul :: Expr -> Expr -> Expr
mul exp1 exp2 = Mul exp1 exp2  

sin :: Expr -> Expr
sin expr = Sin expr

cos :: Expr -> Expr
cos expr = Cos expr

size :: Expr -> Int
size X = 0
size (Num n) = 0
size (Cos expr) = size(expr) + 1
size (Sin expr) = size(expr) + 1
size expr = size expr 

showMul :: Expr -> String 
showMul (Add exp1 exp2) = "(" ++ showExpr exp1 ++ " + " ++ showExpr exp2 ++ ")"
showMul exp = showExpr exp

showFunc :: Expr -> String
showFunc (Add exp1 exp2) = "(" ++ "(" ++ showExpr exp1 ++ " + " ++ showExpr exp2 ++ ")" ++ ")"
showFunc (Mul exp1 exp2) = "(" ++ "(" ++ showExpr exp1 ++ ")" ++ "*" ++ "(" ++ showExpr exp2 ++ ")" ++ ")"
showFunc (Sin exp)      = "(" ++ "sin " ++ "(" ++ showExpr exp ++ ")" ++ ")"
showFunc (Cos exp)      = "(" ++ "cos" ++ "(" ++ showExpr exp ++ ")" ++ ")"
showFunc exp = showExpr exp

showExpr :: Expr -> String
showExpr (Num n) = show n
showExpr (Mul (Add exp1 exp2) exp)  = showMul (Add exp1 exp2) ++ "*" ++showMul exp 
showExpr (Mul exp (Add exp1 exp2))  = showMul exp ++ "*" ++ showMul (Add exp1 exp2)
showExpr (Add exp1 exp2) = showExpr exp1 ++ " + " ++ showExpr exp2
showExpr (Mul exp1 exp2) = showMul exp1 ++ "*" ++ showExpr exp2

showExpr (Sin exp)       = "sin " ++ showFunc exp
showExpr (Cos exp)       = "cos " ++ showFunc exp 
showExpr (X)             = "x"

evalFactor :: Expr -> Double -> Double
evalFactor (Add exp1 exp2) d = (eval exp1 d) + (eval exp2 d)
evalFactor exp d = eval exp d

eval :: Expr -> Double -> Double
eval X d = d
eval (Num n) d = n
eval (Add exp1 exp2) d = (eval exp1 d) + (eval exp2 d)
eval (Mul exp1 exp2) d = (evalFactor exp1 d) * (evalFactor exp2 d)
eval (Sin exp) d       = Prelude.sin (eval exp d)
eval (Cos exp) d       = Prelude.cos (eval exp d)


expr, term, factor,funSin,funCos,varX :: Parser Expr
expr     = foldl1 Add <$> chain term (char '+')
term     = foldl1 Mul <$> chain factor (char '*')
funSin   =  do
            char 's'
            char 'i'
            char 'n'
            Sin <$> factor
funCos  = do
            char 'c'
            char 'o'
            char 's'
            Cos <$> factor
varX = do
            char 'x'
            return X
factor = Num <$> readsP
        <|> funSin
        <|> funCos
        <|> varX
        <|> do
            char '('
            e <- expr
            char ')'
            return e

readExpr :: String -> Maybe Expr
readExpr s = Just (fst (fromJust (parse expr (filter (not.isSpace) s))))
        
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr exp = x <= 0.000001 && x >= (-0.000001)
                        where x = eval exp 1 - eval (fromJust (readExpr (showExpr exp))) 1



arbOp :: Gen Expr -> Gen Expr  -> Gen Expr
arbOp e1 e2  = do e1' <- e1
                  e2' <- e2
                  elements[Add e1' e2', Mul e1' e2']
arbExpr :: Int -> Gen Expr
arbExpr i = frequency [(1,genNum),(i,arbOp (arbExpr $ i `div` 2) (arbExpr $ i `div` 2)), (i, arbFun (arbExpr $ i `div` 2))]


arbFun :: Gen Expr -> Gen Expr
arbFun e = do
            e' <- e
            elements[Cos e', Sin e']

genNum :: Gen Expr
genNum =  do
          Num <$> arbitrary



instance Arbitrary Expr where
    arbitrary = sized arbExpr


simplify :: Expr -> Expr
simplify (Num n) = Num n
simplify X = X

simplify (Add (Num 0) expr)         = simplify expr
simplify (Add expr (Num 0))         = simplify expr
simplify (Add (Num n1) (Num n2))    = Num (eval (Add (Num n1) (Num n2)) 1)
simplify (Add expr X)               = Add (simplify expr) X
simplify (Add X expr)               = Add (simplify expr) X
simplify (Add (Sin X) expr)         = Add (Sin X) (simplify expr)
simplify (Add (Cos X) expr)         = Add (Cos X) (simplify expr)
simplify (Add expr1 expr2)          = simplify (Add (simplify expr1) (simplify expr2))

simplify (Mul expr (Num 0))         = Num 0
simplify (Mul (Num 0) expr)         = Num 0
simplify (Mul (Num 1) expr)         = simplify expr
simplify (Mul expr (Num 1))         = simplify expr
simplify (Mul expr X)               = Mul (simplify expr) X
simplify (Mul X expr)               = Mul (simplify expr) X
simplify (Mul (Num n1) (Num n2))    = Num (eval (Mul (Num n1) (Num n2)) 1)
simplify (Mul (Sin X) expr)         = Mul (Sin X) (simplify expr)
simplify (Mul (Cos X) expr)         = Mul (Cos X) (simplify expr)
simplify (Mul expr1 expr2)          = simplify (Mul (simplify expr1) (simplify expr2))


simplify (Sin (Num n))              = Num (eval (Sin (Num n)) 1)
simplify (Cos (Num n))              = Num (eval (Cos (Num n)) 1)
simplify (Sin (expr))               = Sin (simplify expr)
simplify (Cos (expr))               = Cos (simplify expr)

differentiate :: Expr -> Expr
differentiate expr = simplify (differentiate' (simplify expr))

differentiate' :: Expr -> Expr

--Addition Rule
differentiate' (Add expr1 expr2)   = (Add (differentiate' expr1) (differentiate' expr2))

--Product Rule
differentiate' (Mul expr1 expr2)   = (Add (Mul expr1 (differentiate' expr2)) (Mul expr2 (differentiate' expr1)))

--Sin rule
differentiate' (Sin expr)          = (Mul (differentiate' expr) (Cos (expr)))

--Cos rule
differentiate' (Cos expr)          = Mul (Mul (differentiate' expr) (Sin (expr))) (Num (-1))

--Derivate of number is 0
differentiate' (Num n)             = Num 0

--Derivate of X is 1
differentiate' (X)                 = Num 1




















