module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.List
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
isSudoku (Sudoku r) = isSudokuHelper (Sudoku r) && length r == 9

isSudokuHelper :: Sudoku -> Bool
isSudokuHelper (Sudoku []) = True
isSudokuHelper (Sudoku (x:xs)) = (length x == 9) && isRow x && isSudokuHelper (Sudoku xs)

isRow :: Row -> Bool
isRow [] = True
isRow (x:xs) = isCell x && isRow xs

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
printRows [] = ""
printRows (x:xs) = cellToChar x : printRows xs

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
stringToRow (x:xs) = [Just (digitToInt x)] ++ stringToRow xs

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
--block 1 = take 3 


blocks :: Sudoku -> [Block]
blocks = undefined

block :: [Row]-> Int -> Block
block [] _ = []
block (x:xs) i = take i (drop (i-3)  x) ++ block xs i    

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths = undefined

-- * D3
blocksToOkay :: [Block] -> Bool
blocksToOkay [] = True
blocksToOkay (x:xs) = isOkayBlock x && blocksToOkay xs

-- Did not have time to implement the 3x3 blocks
isOkay :: Sudoku -> Bool
isOkay (Sudoku s) = blocksToOkay (s ++ transpose s)


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks = undefined

--prop_blanks_allBlanks :: ...
--prop_blanks_allBlanks =


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = undefined

--prop_bangBangEquals_correct :: ...
--prop_bangBangEquals_correct =


-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update = undefined

--prop_update_updated :: ...
--prop_update_updated =


------------------------------------------------------------------------------

-- * F1


-- * F2


-- * F3


-- * F4
