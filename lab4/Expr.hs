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
simplify (Add expr1 expr2)          =  (Add (simplify expr1) (simplify expr2))

simplify (Mul expr (Num 0))         = Num 0
simplify (Mul (Num 0) expr)         = Num 0
simplify (Mul (Num 1) expr)         = simplify expr
simplify (Mul expr (Num 1))         = simplify expr
simplify (Mul expr X)               = Mul (simplify expr) X
simplify (Mul X expr)               = Mul (simplify expr) X
simplify (Mul (Num n1) (Num n2))    = Num (n1 * n2)
simplify (Mul expr1 expr2)          = simplify (Mul (simplify expr1) (simplify expr2))

simplify (Sin (Num n))              = Num (sin n) 
simplify (Cos (Num n))              = Num (cos n)
simplify (Sin (expr))               = simplify (Sin (simplify expr))
simplify (Cos (expr))               = simplify (Cos (simplify expr))

differentiate :: Expr -> Expr
differentiate (Add (Num n) (Num n2)) = Num 0
differentiate (Add X (Num n2))       = Num 1
differentiate (Add (Num n2) X)       = Num 1
differentiate (Add X X)              = Num 2
differentiate (Add expr X)           = simplify (Add expr (Num 1))
differentiate (Add X expr)           = simplify (Add expr (Num 1))
differentiate (Add expr1 expr2)      = differentiate (simplify (Add (expr1) (expr2)))

differentiate (Mul (Num n) (Num n2)) = simplify (Num 0)
differentiate (Mul X (Num n))        = Num n
differentiate (Mul (Num n) X)        = Num n
differentiate (Mul X X)              = Mul X (Num 2)
differentiate (Mul expr X)           = simplify expr
differentiate (Mul X expr)           = simplify expr
differentiate (Mul expr1 expr2)      = differentiate (simplify (Mul expr1 expr2))


















