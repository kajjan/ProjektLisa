-----------------------------------------------------------------------------------------------------------------------
-- Setting up the modules for the script


import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import Data.IORef

import Expr

import Data.Maybe
import Text.Read hiding (get)
-------------------------------------------------------------------------------------------------------------------------
-- Setting up the helper functions for the plotter

canWidth,canHeight :: Num a => a                                           -- size for canvas elements
canWidth  = 300
canHeight = 300


realToPix :: Double -> Point -> Double -> Double
realToPix r (x,y) scale = r / (0.04/scale) + x

pixToReal :: Double -> Point -> Double -> Double
pixToReal p (x,y) scale = negate ((p - y) * (0.04/scale))

points :: Expr -> Double -> Point -> (Int,Int) -> [Point]
points ex scale center (width,height) = map (\x ->
                                      (x,realToPix (eval ex (pixToReal x center scale)) center scale))
                                      [0..(fromIntegral width)]



lineStep :: [a] -> [(a, a)]
lineStep (x:y:[]) = [(x, y)]
lineStep (x:y:xs) = (x, y) : lineStep (y:xs)


lines' :: Expr -> Double -> (Double, Double) -> (Int,Int) -> [(UI.Point, UI.Point)]
lines' expr scale center canSize = lineStep $ points expr scale center canSize

-----------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas  <- mkCanvas canWidth canHeight   -- The drawing area
     fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
     input   <- mkInput 20 "x"                -- The formula input
     draw    <- mkButton "Draw graph"         -- The draw button
     diff    <- mkButton "Differentiate"      -- Differentiate button
     zoomOut <- mkButton "Zoom out"           -- Zoom out graph 
     
     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input]
     getBody window #+ [column [pure canvas,pure formula,pure draw,pure zoomOut,pure diff]]

     -- Styling
     getBody window # set style [("backgroundColor","green"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]

     -- Interaction (install event handlers)
     on UI.click     draw    $ \ _ -> readAndDraw input canvas 1.0 (canWidth/2,canHeight/2)
     on valueChange' input   $ \ _ -> readAndDraw input canvas 1.0 (canWidth/2,canHeight/2)
     on UI.click     zoomOut $ \ _ -> readAndDraw input canvas 1.0 (canWidth/2,canHeight/2)
     on mousedown'   canvas  $ \ center -> readAndDraw input canvas 2.0 center
     on UI.click     diff    $ \ _ -> do 
      difFunc input 
      readAndDraw input canvas 1.0 (canWidth/2,canHeight/2)

---------------------------------------------------------------------------------------------------------------------------
-- | creates the plot from input and scale | default set to 1 if not zoomed in or out

readAndDraw :: Element -> Canvas -> Double -> Point -> UI ()
readAndDraw input canvas scale center =
  do -- Get the current formula (a String) from the input element
     formula <- get value input
     -- Clear the canvas
     clearCanvas canvas
  
     case readExpr formula of
      Just expr -> do
        plotExpr expr

      Nothing -> do
        return()

      where
        plotExpr expr1 = (id
          . drawLines
          . lines' expr1 scale center) (canWidth, canHeight)

        
        drawLines [] = return ()
        drawLines ((p1, p2):xs) = do
            UI.beginPath canvas

            UI.moveTo p1 canvas
            UI.lineTo p2 canvas

            UI.closePath canvas
            UI.stroke canvas

            drawLines xs

 
-- | generates the differentiated function
difFunc :: Element -> UI ()
difFunc input = do
               fx <- get value input
               case readExpr fx of
                   Just expr -> do
                                  let dfxdt = differentiate  expr
                                  set value (showExpr dfxdt) (pure input)
                                  return ()
                   Nothing -> do
                                return ()                           
