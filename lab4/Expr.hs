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


expr, term, factor, funSin :: Parser Expr
expr     = foldl1 Add <$> chain term (char '+')      
term     = foldl1 Mul <$> chain factor (char '*')

funSin   = do
            char 's'
            char 'i'
            char 'n'
            Sin <$> factor
funCos   = do
            char 'c'
            char 'o'
            char 's'
            Cos <$> factor
varX     = do
            char 'x'
            return X
factor   = Num <$> readsP 
  <|> funCos
  <|> funSin
  <|> do
  char '(' *> expr <* char ')'

readStr :: String -> Maybe Expr
readStr s = Just (fst (fromJust (parse expr (filter (not.isSpace) s))))
        
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr exp = exp == fromJust (readStr (showExpr exp))


genNum   :: Gen Expr
genNum = do
         x <- arbitrary
         return (Num x)

simplify :: Expr -> Expr
simplify (Num n) = Num n
simplify X = X

simplify (Add (Num 0) expr)         = simplify expr
simplify (Add expr (Num 0))         = simplify expr
simplify (Add (Num n1) (Num n2))    = Num (n1 + n2)
simplify (Add expr X)               = (Add (simplify expr) X)
simplify (Add X expr)               = (Add (simplify expr) X)
simplify (Add expr1 expr2)          = (Add (simplify expr1) (simplify expr2))

simplify (Mul expr (Num 0))         = Num 0
simplify (Mul (Num 0) expr)         = Num 0
simplify (Mul (Num 1) expr)         = simplify expr
simplify (Mul expr (Num 1))         = simplify expr
simplify (Mul expr X)               = Mul (simplify expr) X
simplify (Mul X expr)               = Mul (simplify expr) X
simplify (Mul (Num n1) (Num n2))    = Num (n1 * n2)
simplify (Mul expr1 expr2)          = simplify (Mul (simplify expr1) (simplify expr2))

simplify (Sin (Num n))              = Num (Prelude.sin n) 
simplify (Cos (Num n))              = Num (Prelude.cos n)
simplify (Sin (expr))               = simplify (Sin (simplify expr))
simplify (Cos (expr))               = simplify (Cos (simplify expr))

differentiate :: Expr -> Expr
differentiate expr = simplify (differentiate' (simplify expr))

differentiate' :: Expr -> Expr

--Addition Rule
differentiate' (Add expr1 expr2)           = (Add (differentiate' expr1) (differentiate' expr1))

--Product Rule
differentiate' (Mul expr1 expr2)           = (Add (Mul expr1 (differentiate' expr2)) (Mul expr2 (differentiate' expr1)))

--Sin rule
differentiate' (Sin expr)          = (Mul (differentiate' expr) (Cos (expr)))

--Cos rule
differentiate' (Cos expr)          = Mul (Mul (differentiate' expr) (Sin (expr))) (Num (-1))

--Derivate of number is 0
differentiate' (Num n)             = Num 0

--Derivate of X is 1
differentiate' (X)                 = Num 1




















