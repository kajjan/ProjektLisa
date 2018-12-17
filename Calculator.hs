-- This module is a starting point for implementing the Graph Drawing
-- Calculator as described in Part II of the Standard Lab. You can use this
-- directly, or just study it as an example of how to use threepenny-gui.
import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import Data.IORef

import Expr

import Data.Maybe
import Text.Read hiding (get)

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
     input   <- mkInput 20 "x"                -- The formula input
     draw    <- mkButton "Draw graph"         -- The draw button
       -- The markup "<i>...</i>" means that the text inside should be rendered
       -- in italics.

     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input]
     getBody window #+ [column [pure canvas,pure formula,pure draw]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]

     -- Interaction (install event handlers)
     on UI.click     draw  $ \ _ -> readAndDraw input canvas
     on valueChange' input $ \ _ -> readAndDraw input canvas

points :: Expr -> Double -> (Int,Int) -> [Point]
points e scale (w,h) = 
    zip l (map realToPix (map (eval e) (map pixToReal l)))
    where
        l = [0 .. w']
    
        w' = fromIntegral w
        h' = fromIntegral h
        
        pixToReal :: Double -> Double
        pixToReal x = (x - (w' / 2)) * scale

        realToPix :: Double -> Double
        realToPix y = ((-y) / scale) + (h' / 2)


readAndDraw :: Element -> Canvas -> UI ()
readAndDraw input canvas =
  do -- Get the current formula (a String) from the input element
     formula <- get value input
     -- Clear the canvas
     clearCanvas canvas
     -- The following code draws the formula text in the canvas and a blue line.
     -- It should be replaced with code that draws the graph of the function.
     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
     case readExpr formula of 
                            (Just exp) -> do
                                              path "blue" (points exp origScale (canHeight,canHeight)) canvas
                                              UI.fillText ((showExpr . simplify ) exp) (10,canHeight/2) canvas

                            _ -> UI.fillText "WRONG" (10,canHeight/2) canvas

                            
zoom  ::  Elem -> Elem -> Double -> Canvas -> IO ()
zoom input scale change canvas = 
    do
        s <- getProp scale "value"
        setProp scale "value" (show ((read s::Double)*change))
        readAndDraw input scale canvas                            