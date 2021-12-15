module Expr where 
import Parsing
import Data.Char ( isSpace )
import Maybes
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

add' :: Expr -> Expr -> Expr
add' exp1 exp2 = Add exp1 exp2

mul' :: Expr -> Expr -> Expr
mul' exp1 exp2 = Mul exp1 exp2

sin' :: Expr -> Expr
sin' expr = Sin expr

cos' :: Expr -> Expr
cos' expr = Cos expr

size :: Expr -> Int
size X = 0
size (Num n) = 0
size (Cos expr) = size(expr) + 1
size (Sin expr) = size(expr) + 1
size expr = size expr

showMul :: Expr -> String
showMul (Add exp1 exp2) = "(" ++ showExpr (Add exp1 exp2) ++ ")"
showMul exp = showExpr exp

showFunc :: Expr -> String
showFunc (Add exp1 exp2) = "(" ++ showExpr exp1 ++ " + " ++ showExpr exp2 ++ ")"
showFunc (Mul exp1 exp2) = "(" ++ showExpr exp1 ++ "*" ++ showExpr exp2 ++ ")"
showFunc exp = showExpr exp

showExpr :: Expr -> String
showExpr (Num n) = show n
showExpr (Add exp1 exp2) = showExpr exp1 ++ " + " ++ showExpr exp2
showExpr (Mul exp1 exp2) = showMul exp1 ++ "*" ++ showExpr exp2
showExpr (Sin exp)       = "sin " ++ showFunc exp
showExpr (Cos exp)       = "cos " ++ showFunc exp
showExpr (X)        = "x"

evalFactor :: Expr -> Double -> Double
evalFactor (Add exp1 exp2) d = (eval exp1 d) + (eval exp2 d)
evalFactor exp d = eval exp d

eval :: Expr -> Double -> Double
eval X d = d
eval (Num n) d = n
eval (Add exp1 exp2) d = (eval exp1 d) + (eval exp2 d)
eval (Mul exp1 exp2) d = (evalFactor exp1 d) * (evalFactor exp2 d)
eval (Sin exp) d       = sin (eval exp d)
eval (Cos exp) d       = cos (eval exp d)


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
readExpr str = Just ( fst (fromJust (parse expr  (filter (not.isSpace) str))))

prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e =  fromJust (readExpr $ showExpr e) ==  (fromJust $ readExpr $ showExpr e)


arbExpr :: Int -> Gen Expr
arbExpr i = frequency [(1,genNum),(i,arbOp (arbExpr $ i `div` 2) (arbExpr $ i `div` 2)), (i, arbFun (arbExpr $ i `div` 2))]


arbFun :: Gen Expr -> Gen Expr
arbFun e = do
            e' <- e
            elements[Cos e', Sin e']

genNum :: Gen Expr
genNum =  do
          Num <$> arbitrary


arbOp :: Gen Expr -> Gen Expr  -> Gen Expr
arbOp e1 e2  = do e1' <- e1
                  e2' <- e2
                  elements[Add e1' e2', Mul e1' e2']

instance Arbitrary Expr where
    arbitrary = sized arbExpr

    --   -475.77975142772766

differentiate :: Expr -> Expr
differentiate = undefined 