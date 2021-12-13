
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

expr, term, factor :: Parser Expr

expr    = do
        t <- term
        ts <- zeroOrMore (do char '+'; term)
        return foldl (Add t ts) 
term = undefined
factor = undefined

readStr :: String -> Maybe Expr
