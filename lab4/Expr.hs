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
num = Num

x :: Expr
x = X

add :: Expr -> Expr -> Expr
add = BinOp Add

mul :: Expr -> Expr -> Expr
mul = BinOp Mul

sin :: Expr -> Expr
sin = Func Sin

cos :: Expr -> Expr
cos = Func Cos

size :: Expr -> Int
size X = 0
size (Num n) = 0
size (Func f expr) = size expr + 1
size (BinOp op expr1 expr2) = (size expr1) + (size expr2) + 1


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
showExpr X                  = "x"


eval :: Expr -> Double -> Double
eval X d = d
eval (Num n) d = n
eval (BinOp Add expr1 expr2) d = eval expr1 d + eval expr2 d
eval (BinOp Mul expr1 expr2) d = eval expr1 d * eval expr2 d
eval (Func Sin expr) d        = Prelude.sin (eval expr d)
eval (Func Cos expr) d        = Prelude.cos (eval expr d)


expr, term, factor,funSin,funCos,varX :: Parser Expr
expr     = foldl1 (BinOp Add) <$> chain term (char '+')
term     = foldl1 (BinOp Mul) <$> chain factor (char '*')
funSin   =  do
            char 's'
            char 'i'
            char 'n'
            Func Sin <$> factor
funCos  = do
            char 'c'
            char 'o'
            char 's'
            Func Cos <$> factor
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

prop_ShowReadExpr :: Expr -> Double -> Bool
prop_ShowReadExpr expr d = x <= 0.000001 && x >= (-0.000001)
                        where x = (eval expr d) - (eval (fromJust (readExpr (showExpr expr))) d)



arbOp :: Gen Expr -> Gen Expr -> Gen Expr
arbOp e1 e2  = do
                  op <- elements[Add,Mul]
                  e1' <- e1
                  BinOp op e1' <$> e2
arbExpr :: Int -> Gen Expr
arbExpr i = frequency [((2),genNum),(i,arbOp (arbExpr $ i `div` 2) (arbExpr $ i `div` 2)), (i, arbFun (arbExpr $ i `div` 2)), ((1),genX) ]


arbFun :: Gen Expr -> Gen Expr
arbFun e = do
            f <- elements[Cos, Sin]
            Func f <$> e

genNum :: Gen Expr
genNum =
          Num <$> arbitrary
genX :: Gen Expr
genX  = do
        (return X)



instance Arbitrary Expr where
    arbitrary = sized arbExpr

simplify :: Expr -> Expr
simplify (Num n) = Num n
simplify X = X

simplify (BinOp Add (Num 0) expr)         = simplify expr
simplify (BinOp Add expr (Num 0))         = simplify expr
simplify (BinOp Add (Num n1) (Num n2))    = Num (eval (add (Num n1) (Num n2)) 1)


simplify (BinOp Mul expr (Num 0))         = Num 0
simplify (BinOp Mul (Num 0) expr)         = Num 0
simplify (BinOp Mul (Num 1) expr)         = simplify expr
simplify (BinOp Mul expr (Num 1))         = simplify expr
simplify (BinOp Mul (Num n1) (Num n2))    = Num (eval (mul (Num n1) (Num n2)) 1)


simplify (Func f (Num n))               = Num (eval (Func f (Num n)) 1)


simplify (BinOp op expr1 expr2) 
    | expr1 == expr1' && expr2 == expr2' = BinOp op expr1 expr2
    | expr1 == expr1' && expr2 /= expr2' = BinOp op expr1 expr2'
    | expr1 /= expr1' && expr2 == expr2' = BinOp op expr1' expr2
    | otherwise = simplify (BinOp op expr1' expr2')
    where expr1' = simplify expr1
          expr2' = simplify expr2

simplify (Func f expr) 
    | expr == expr' = Func f expr
    | otherwise = Func f (simplify expr')
    where expr' = simplify expr

differentiate :: Expr -> Expr
differentiate expr = simplify (differentiate' (simplify expr))

differentiate' :: Expr -> Expr

--Addition Rule
differentiate' (BinOp Add expr1 expr2)   =  add (differentiate' expr1) (differentiate' expr2)

--Product Rule
differentiate' (BinOp Mul expr1 expr2)   =  add (mul expr2 (differentiate' expr1)) (mul expr1 (differentiate' expr2))

--Sin rule
differentiate' (Func Sin expr)           =  mul (Func Cos expr) (differentiate' expr) 

--Cos rule
differentiate' (Func Cos expr)           =  mul (Num (-1)) (mul (Func Sin expr) (differentiate' expr))

--Derivate of number is 0
differentiate' (Num n)                   = Num 0

--Derivate of X is 1
differentiate' X                         = Num 1




















