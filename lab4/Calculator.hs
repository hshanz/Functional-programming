import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import Expr
import GHC.Base 
import Expr 
import Data.Maybe 
import Data.ByteString 

canWidth,canHeight :: Num a => a
canWidth  = 300
canHeight = 300

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas  <- mkCanvas canWidth canHeight   -- The drawing area
     fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
     input   <- mkInput 20 "x"
     zoomInput <- mkSlider (1, 100) 50              -- The formula input
     draw    <- mkButton "Draw graph"  
     diffButton <- mkButton "Differentiate"       -- The draw button
       -- The markup "<i>...</i>" means that the text inside should be rendered
       -- in italics.

     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input]
     getBody window #+ [column [pure canvas,pure formula,pure draw,pure zoomInput,pure diffButton]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]

     -- Interaction (install event handlers)
     on UI.click     draw        $ \ _ -> readAndDraw input zoomInput canvas False
     on valueChange' input       $ \ _ -> readAndDraw input zoomInput canvas False
     on valueChange' input       $ \ _ -> readAndDraw input zoomInput canvas False
     on valueChange' zoomInput   $ \ _ -> readAndDraw input zoomInput canvas False
     on UI.click     diffButton  $ \ _ -> readAndDraw input zoomInput canvas True


readAndDraw :: Element -> Element -> Canvas -> Bool -> UI ()
readAndDraw input zoomInput canvas diffClicked =
  do -- Get the current formula (a String) from the input element
     formula <- get value input
     zoom <- get value zoomInput
    
     -- Clear the canvas
     clearCanvas canvas
     let zoom' = read zoom / 1250

     let inputField 
            | diffClicked = differentiate $ fromJust $ readExpr formula
            | otherwise = fromJust $ readExpr formula
     -- The following code draws the formula text in the canvas and a blue line.
     -- It should be replaced with code that draws the graph of the function.
     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
     UI.fillText (showExpr inputField) (10,canHeight/2) canvas
     path "blue" (points inputField zoom' (300,300)) canvas

points :: Expr -> Double -> (Int,Int) -> [Point]
points exp s (w,h) = [(fromIntegral n, translatePoints h s (eval exp (s * fromIntegral (n-150) ))) | n <-[0..w] ]

translatePoints :: Int -> Double -> Double -> Double
translatePoints h s y = origo - (y/s)
                where origo = fromIntegral (h `div` 2)