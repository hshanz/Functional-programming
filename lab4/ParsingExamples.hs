-- | Parsing
-- Examples to illustrate how to write parsers using parsing combinators
-- Functional Programming course 2019.
-- David Sands

module ParsingExamples where
import Data.Char(isDigit)
import Parsing hiding (chain,digit)

import Control.Monad(forever)

import Data.Char(isSpace)
import Data.Maybe
import Test.QuickCheck


--------------------------------------------------------------------------------
-- * A first example
-- Writing a recursive decent parser directly
-- Using functions of type String -> Maybe (a,String)

type ParserFun a = String -> Maybe (a,String)

{- EBNF:
digit = "0".."9".
number = digit{digit}.
addition = number "+" number.
-}

num :: ParserFun Integer

num s = case span isDigit s of
    ("",_)    -> Nothing
    (ds,rest) -> Just(read ds, rest)
    
 {- case takeWhile isDigit s of
             "" -> Nothing
             ds -> Just(read ds,dropWhile isDigit s)
  -}
-- use span

addition0 :: ParserFun Integer
addition0 s = case num s of
   Just(n,'+':rest) -> case num rest of
                            Just (m, rest') -> Just (n + m, rest')
                            Nothing         -> Nothing
   _                -> Nothing
                    
multiplication0 s = case num s of
   Just(n,'*':rest) -> case num rest of
                            Just (m, rest') -> Just (n * m, rest')
                            Nothing         -> Nothing
   _                -> Nothing
   
{- A small extension to the EBNF
multiplication ::= number "*" number.
calculation    ::= addition | multiplication.
-}

          -- Pattern: alternative


calculation0 s = case addition0 s of
                      Nothing      -> multiplication0 s
                      ok           -> ok
 --

-- 
 --                   Just(n,"")  -> Just n
 --                     _            -> Nothing
                      
                      


--------------------------------------------------------------------------------
-- * Rewriting our first warmup example using parsing combinators
-- (Parsing.hs)

-- | Parse a digit (also available in the Parsing module)
digit :: Parser Char
digit = sat isDigit

-- | Parse a number
number :: Parser Integer
number =  read <$> oneOrMore digit
--   do
--       ds <- oneOrMore digit
--       return (read ds)

-- | Parse two numbers, separated by +, and add them
addition :: Parser Integer
addition = operator '+' (+)
    
-- | Parse two numbers, separated by *, and multiply them
multiplication :: Parser Integer
multiplication = operator '*' (*)

operator c op = do
    n <- number
    char c
    m <- number
    return (n `op` m)

calculation :: Parser Integer
calculation = addition <|> multiplication

--------------------------------------------------------------------------------
-- * An expression parser (version 1)

data Expr = Num Integer
          | Add Expr Expr
          | Mul Expr Expr
          deriving (Eq,Show)

eval :: Expr -> Integer
eval (Num n) = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

{- EBNF:
expr   ::= term {"+" term}.
term   ::= factor {"*" factor}.
factor ::= number | "(" expr ")".
-}

expr, term, factor :: Parser Expr

expr   = foldl1 Add <$> chain term (char '+')
term   = foldl1 Mul <$> chain factor (char '*')
factor = Num <$> number  <|>   char '(' *> expr <* char ')'

{- -- simplifications
      do
           char '('
           e <- expr
           char ')'
           return e
 -}         
-- do
--    t <- term
--    ts <- zeroOrMore (do char '+'; term)
--    return $ foldl Add t ts
--   do
--      ts <- chain factor (char '*')
--      return (foldl Mul ts)
      
 --   t <- factor
 --   ts <- zeroOrMore (do char '*'; factor)
 --   return $ foldl Mul t ts


-- library function:
chain :: Parser item -> Parser sep -> Parser [item]
chain item sep = do
    i <- item
    is <- zeroOrMore (do sep; item)
    return (i:is)



--------------------------------------------------------------------------------
-- * The simple calculator example

main = do putStrLn "Welcome to the simple calculator!"
          forever readEvalPrint

readEvalPrint = do
    putStr "What would you like to calculate?"
    s <- getLine
    let s' = filter (not . isSpace) s
    case parse expr s' of
       Just (e, "") -> print $ eval e
       Nothing      -> putStrLn "Invalid Expression!"




-- Testing
-----------------------------------------------
rExpr :: Int -> Gen Expr
rExpr s = frequency [(1,rNum),(s,rBin s)]  where 
   range = 4 -- integer range
   rNum = elements $ map Num [0..range] -- non negative!
   rBin s = do 
        let s' = (s `div` 2)
        op <- elements [Mul,Add]
        e1 <- rExpr s' 
        e2 <- rExpr s'
        return $ op e1 e2   

instance Arbitrary Expr where
  arbitrary = sized rExpr

prop_readExpr e = eval e == eval e'
    where Just (e',"") = parse expr (showExpr e)
          -- test fails if pattern does not match

showExpr (Num n) = show n
showExpr (Add e1 e2) = showExpr e1   ++ "+" ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ "*" ++ showFactor e2
  where showFactor e@(Add e1 e2) = "(" ++ showExpr e ++ ")"
        showFactor e             = showExpr e

