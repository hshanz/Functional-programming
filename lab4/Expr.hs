module Expr where
import Parsing
import Data.Char
import Data.Maybe
import Test.QuickCheck
data Expr
    = Num Double
    | BinOp Op Expr Expr
    | Func F Expr
    | X
    deriving (Show, Ord, Eq)
data Op = Add | Mul
    deriving (Show, Ord, Eq)
data F = Sin | Cos
    deriving (Show, Ord, Eq)

num :: Double -> Expr
num n = Num n

x :: Expr
x = X

add :: Expr -> Expr -> Expr
add expr1 expr2 = BinOp Add expr1 expr2

mul :: Expr -> Expr -> Expr
mul expr1 expr2 = BinOp Mul expr1 expr2  

sin :: Expr -> Expr
sin expr = Func Sin expr

cos :: Expr -> Expr
cos expr = Func Cos expr

size :: Expr -> Int
size X = 0
size (Num n) = 0
size (Func Cos expr) = size(expr) + 1
size (Func Sin expr) = size(expr) + 1
size expr = size expr 

showMul :: Expr -> String 
showMul (BinOp Add expr1 expr2) = "(" ++ showExpr expr1 ++ " + " ++ showExpr expr2 ++ ")"
showMul expr = showExpr expr

showFunc :: Expr -> String
showFunc (BinOp Add expr1 expr2) = "(" ++ "(" ++ showExpr expr1 ++ " + " ++ showExpr expr2 ++ ")" ++ ")"
showFunc (BinOp Mul expr1 expr2) = "(" ++ "(" ++ showExpr expr1 ++ ")" ++ "*" ++ "(" ++ showExpr expr2 ++ ")" ++ ")"
showFunc (Func Sin expr)        = "(" ++ "sin " ++ "(" ++ showExpr expr ++ ")" ++ ")"
showFunc (Func Cos expr)        = "(" ++ "cos" ++ "(" ++ showExpr expr ++ ")" ++ ")"
showFunc expr = showExpr expr

showExpr :: Expr -> String
showExpr (Num n) = show n
showExpr (BinOp Mul (BinOp Add expr1 expr2) expr)  = showMul (add expr1 expr2) ++ "*" ++showMul expr 
showExpr (BinOp Mul expr (BinOp Add expr1 expr2))  = showMul expr ++ "*" ++ showMul (add expr1 expr2)
showExpr (BinOp Add expr1 expr2) = showExpr expr1 ++ " + " ++ showExpr expr2
showExpr (BinOp Mul expr1 expr2) = showMul expr1 ++ "*" ++ showExpr expr2

showExpr (Func Sin expr)       = "sin " ++ showFunc expr
showExpr (Func Cos expr)       = "cos " ++ showFunc expr
showExpr (X)                  = "x"

evalFactor :: Expr -> Double -> Double
evalFactor (BinOp Add expr1 expr2) d = (eval expr1 d) + (eval expr2 d)
evalFactor expr d = eval expr d

eval :: Expr -> Double -> Double
eval X d = d
eval (Num n) d = n
eval (BinOp Add expr1 expr2) d = (eval expr1 d) + (eval expr2 d)
eval (BinOp Mul expr1 expr2) d = (evalFactor expr1 d) * (evalFactor expr2 d)
eval (Func Sin expr) d        = Prelude.sin (eval expr d)
eval (Func Cos expr) d        = Prelude.cos (eval expr d)


expr, term, factor,funSin,funCos,varX :: Parser Expr
expr     = foldl1 (BinOp Add) <$> chain term (char '+')
term     = foldl1 (BinOp Mul) <$> chain factor (char '*')
funSin   =  do
            char 's'
            char 'i'
            char 'n'
            (Func Sin) <$> factor
funCos  = do
            char 'c'
            char 'o'
            char 's'
            (Func Cos) <$> factor
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
prop_ShowReadExpr expr = x <= 0.000001 && x >= (-0.000001)
                        where x = eval expr 1 - eval (fromJust (readExpr (showExpr expr))) 1



arbOp :: Gen Expr -> Gen Expr -> Gen Expr
arbOp e1 e2  = do 
                  op <- elements[Add,Mul]  
                  e1' <- e1
                  e2' <- e2
                  return (BinOp op e1' e2')
arbExpr :: Int -> Gen Expr
arbExpr i = frequency [(1,genNum),(i,arbOp (arbExpr $ i `div` 2) (arbExpr $ i `div` 2)), (i, arbFun (arbExpr $ i `div` 2))]


arbFun :: Gen Expr -> Gen Expr
arbFun e = do
            f <- elements[Cos, Sin]
            e' <- e
            return (Func f e')

genNum :: Gen Expr
genNum =  do
          Num <$> arbitrary

instance Arbitrary Expr where
    arbitrary = sized arbExpr

simplify :: Expr -> Expr
simplify (Num n) = Num n
simplify X = X

simplify (BinOp Add (Num 0) expr)         = simplify expr
simplify (BinOp Add expr (Num 0))         = simplify expr
simplify (BinOp Add (Num n1) (Num n2))    = Num (eval (add (Num n1) (Num n2)) 1)
simplify (BinOp Add expr X)               = add (simplify expr) X
simplify (BinOp Add X expr)               = add (simplify expr) X
simplify (BinOp Add (Func f X) expr)      = add (Func f X) (simplify expr)
simplify (BinOp Add expr1 expr2)          = simplify (add (simplify expr1) (simplify expr2))

simplify (BinOp Mul expr (Num 0))         = Num 0
simplify (BinOp Mul (Num 0) expr)         = Num 0
simplify (BinOp Mul (Num 1) expr)         = simplify expr
simplify (BinOp Mul expr (Num 1))         = simplify expr
simplify (BinOp Mul expr X)               = mul (simplify expr) X
simplify (BinOp Mul X expr)               = mul (simplify expr) X
simplify (BinOp Mul (Num n1) (Num n2))    = Num (eval (mul (Num n1) (Num n2)) 1)
simplify (BinOp Mul (Func f X) expr)      = mul (Func f X) (simplify expr)
simplify (BinOp Mul expr1 expr2)          = simplify (mul (simplify expr1) (simplify expr2))


simplify (Func Sin (Num n))               = Num (eval (Func Sin (Num n)) 1)
simplify (Func Cos (Num n))               = Num (eval (Func Cos (Num n)) 1)
simplify (Func Sin (expr))                = Func Sin (simplify expr)
simplify (Func Cos (expr))                = Func Cos (simplify expr)

differentiate :: Expr -> Expr
differentiate expr = simplify (differentiate' (simplify expr))

differentiate' :: Expr -> Expr

--Addition Rule
differentiate' (BinOp Add expr1 expr2)   =  add (differentiate' expr1) (differentiate' expr2)

--Product Rule
differentiate' (BinOp Mul expr1 expr2)   =  add (mul expr1 (differentiate' expr2)) (mul expr2 (differentiate' expr1))

--Sin rule
differentiate' (Func Sin expr)           =  mul (differentiate' expr) (Func Cos (expr))

--Cos rule
differentiate' (Func Cos expr)           =  mul (mul (differentiate' expr) (Func Sin (expr))) (Num (-1))

--Derivate of number is 0
differentiate' (Num n)             = Num 0

--Derivate of X is 1
differentiate' (X)                 = Num 1




















