module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.List
import Data.Maybe
------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row]
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

allFilled :: Sudoku
allFilled = Sudoku (replicate 9 (replicate 9 (Just 1)))
-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku r) = length r == 9 && all isCell (concat r)

isCell :: Cell -> Bool
isCell Nothing = True
isCell (Just n) = n <= 9 && n >= 1

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku [])     = True
isFilled (Sudoku (x:xs)) = isFilledHelper x && isFilled (Sudoku xs)

isFilledHelper :: Row -> Bool
isFilledHelper []           = True
isFilledHelper (Nothing:xs) = False
isFilledHelper (x:xs)       = isFilledHelper xs

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku sud =  putStr (printSudHelper sud)

printSudHelper :: Sudoku -> String
printSudHelper (Sudoku []) = ""
printSudHelper (Sudoku (x:xs)) = printRows x ++ ['\n'] ++ printSudHelper (Sudoku xs)

printRows :: Row -> String
printRows = map cellToChar

cellToChar :: Cell -> Char
cellToChar Nothing = '.'
cellToChar (Just n) = intToDigit n

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku sud = do
  c <- readFile sud
  return (Sudoku( convertStringToSud (lines c)))

convertStringToSud :: [String] -> [Row]
convertStringToSud = map stringToRow

stringToRow :: String -> Row
stringToRow [] = []
stringToRow ('.':xs) = [Nothing] ++ stringToRow xs
stringToRow (x:xs) | isDigit x = [Just (digitToInt x)] ++ stringToRow xs
                   | otherwise = error "Not valid"

------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen Cell
cell = frequency [(9, genNothing ), (1,cellNumbers)]


cellNumbers :: Gen (Maybe Int)
cellNumbers = do
  n <- choose ((1, 9))
  return (Just n)

genNothing :: Gen (Maybe Int)
genNothing = return Nothing


-- * C2
-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = do
      n <- vectorOf 9 (vectorOf 9 cell)
      return (Sudoku n)

 -- hint: get to know the QuickCheck function vectorOf

-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku
  -- hint: this definition is simple!

------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1

isOkayBlock :: Block -> Bool
isOkayBlock b = length (filter (/= Nothing) b) == length (nub (filter (/= Nothing) b))

-- * D2
blocks :: Sudoku -> [Block]
blocks (Sudoku r) = concat ([createBlock (block r x) | x <- [3 * n | n <- [1 .. 3]]])
                  ++ transpose r ++ r



block :: [Row]-> Int -> Block
block [] _ = []
block (x:xs) i = drop (i-3) (take i  x) ++ block xs i

createBlock :: Block -> [Block]
createBlock b  = [take 9 b] ++ [take 9 (drop 9 b)] ++ [drop 18 b]

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths s  = length (blocks s) == 27 &&
                         all (==9) (map length (blocks s))

-- * D3
isOkay :: Sudoku -> Bool
isOkay s = all isOkayBlock (blocks s)



---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks (Sudoku s) = [(r,c) | (r, row) <- zip [0..] s,
                             (c,cell) <- zip [0..] row,
                             cell == Nothing]

prop_blanks_allBlanks :: Bool
prop_blanks_allBlanks = length ( blanks allBlankSudoku )== 81


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
[] !!= (i,y) = [y]
xs !!= (i,y) 
          | length xs < i = error "index out of bounds" 
          | otherwise = hs ++ [y] ++ tail ls 
        where (hs,ls) = splitAt i xs

prop_bangBangEquals_correct :: Row -> (Int,Cell) -> Bool
prop_bangBangEquals_correct row (i,c) = (row !!=(i',c)!!i') == c
                                      where i' = abs (i `mod` (length (row) + 1))
                    
-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update (Sudoku s) (i,j) n = Sudoku(xs ++ [xc] ++ tail xl)
                  where xc = head xl !!= (j,n)
                        (xs, xl) = splitAt i s


prop_update_updated :: Sudoku -> Pos -> Cell -> Bool
prop_update_updated sud (i,j) n = rows (update sud (i',j') n) !! i' !! j' == n 
                                where i' = abs (i `mod` 9)
                                      j' = abs (j `mod` 9)


------------------------------------------------------------------------------

-- * F1
solve :: Sudoku -> Maybe Sudoku
solve sud |isSudoku sud = solve' sud (blanks sud)
          | otherwise = Nothing

solve' :: Sudoku -> [Pos] -> Maybe Sudoku
solve' sud []   | isOkay sud && isFilled sud = Just sud
                | otherwise = Nothing
solve' sud (x:xs) =  head' [(solve' (update sud x (Just n)) xs) | n <- validUpdates sud x]

head' :: [Maybe Sudoku] -> Maybe Sudoku
head' (Nothing:xs) = head' xs
head' ((Just n):xs) = Just n
head' [] = Nothing

validUpdates :: Sudoku -> Pos -> [Int]
validUpdates sud pos = filter (validUpdatesHelper sud pos) [1..9]

validUpdatesHelper :: Sudoku -> Pos -> Int -> Bool
validUpdatesHelper sud pos i = isOkay(update sud pos (Just i))

-- * F2
readAndSolve :: FilePath -> IO ()
readAndSolve f = do
          s <- readSudoku f
          if solve s == Nothing 
            then putStrLn "No solution"
          else printSudoku(fromJust(solve s))

-- * F3
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf s1 s2 = isOkay s1 && isFilled s1 && blanksTest s1 (blanks s2) == s2

blanksTest :: Sudoku -> [Pos] -> Sudoku
blanksTest s   []   = s 
blanksTest s (x:xs) = blanksTest (update s x Nothing) xs 

-- * F4
prop_SolveSound :: Sudoku -> Property
prop_SolveSound s = isJust (solve s) ==> fromJust (solve s) `isSolutionOf` s 

fewerChecks prop =
  quickCheckWith stdArgs{maxSuccess=30 } prop