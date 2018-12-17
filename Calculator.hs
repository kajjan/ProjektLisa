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
canWidth  = 350
canHeight = 350


realToPix :: Double -> Double
realToPix r = r / 0.05 + fromIntegral canWidth / 2

pixToReal :: Double -> Double
pixToReal p = (p - fromIntegral canWidth / 2) * 0.05


points :: Expr -> Double -> (Int,Int) -> [UI.Point]
points expr sl (width, height) = map point [0..width]
    where
        point :: Int -> (Double, Double)  -- denna borde vi nog förstå 
        point x = (fromIntegral x, pointY x)
        pointY = realToPix
            . negate
            . (*sl)
            . (eval expr)
            . (/sl)
            . pixToReal
            . fromIntegral

-- Somehowe moves the lines/dots? We should write something else            
lineStep :: [a] -> [(a, a)]
lineStep (x:y:[]) = [(x, y)]
lineStep (x:y:xs) = (x, y) : lineStep (y:xs)

-- Help for the one above?
lines' :: Expr -> Double -> (Int,Int) -> [(UI.Point, UI.Point)]
lines' expr sl canSize = lineStep $ points expr sl canSize

-- THIS MIGHT BE ONÖDIG testade ett ta bort och inget verkade hända?
readDouble :: String -> Maybe Double
readDouble = readMaybe

convertInput :: String -> String -> Maybe (Expr, Double)
convertInput exprText exprScale = (,) <$> readExpr exprText <*> readDouble exprScale

-----------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Elements
     canvas     <- mkCanvas canWidth canHeight   -- Drawing area
     fx         <- mkHTML "<i>f</i>(<i>x</i>)="  -- Text "f(x)="
     input      <- mkInput 20 "x"                -- Formula input
     draw       <- mkButton "Draw Graph"         -- Draw button
     diff       <- mkButton "Differentiate"      -- Differentiate button
     clr        <- mkButton "Clear Graph"        -- Clears graph
     zoomOut    <- mkButton "Zoom out"           -- Zoom out graph 
     zoomIn     <- mkButton "Zoom in"            -- Zoom in graph
     --normScale  <- mkButton "Normal"             -- Scale to normal     -- TA BORT?
     top        <- UI.div                        -- Page heading
                   #+  [ UI.h1     # set UI.text "Graph Generator"]
     return window # set UI.title "TDA452 - Calculator lab"
     let scaler = 1.0    
     -- Layout
     formula <- row [pure fx,pure input]
     getBody window #+ [pure top,column [pure canvas, pure formula, pure draw, row[pure zoomIn, pure zoomOut],pure diff,pure clr]]  

     -- Styling
     getBody window # set style [("backgroundColor","yellow"),
                                 ("textAlign","center")]
     pure canvas    # set style [("textAlign","center")]
     pure input     # set style [("fontSize","20pt")]
     pure clr       # set style [("backgroundColor","light grey")]

     -- Interaction
     on UI.click     draw  $ \ _ -> readAndDraw input 1.0 canvas               --draws the plot for the given function
     on UI.click     diff  $ \ _ -> do                                         --draws the plot of differentiated input function
      ddt input
      readAndDraw input 1.0 canvas

     on UI.click     clr $ \_    -> do                                         --clears the canvas
                                      UI.clearCanvas canvas
                                      set value ("x") (pure input)

     on UI.click     zoomIn   $ \_ -> readAndDraw input (scaler/0.5) canvas   --zooms in the plot by a factor of 2
     on UI.click     zoomOut $ \_ -> readAndDraw input (scaler*0.5) canvas   --zooms out of the plot by a factor of 2
    -- on UI.click     normScale $ \_ -> readAndDraw input (scaler) canvas       --normalizes the zoomed plots
     on valueChange' input     $ \ _ -> readAndDraw input 1.0 canvas           

---------------------------------------------------------------------------------------------------------------------------
-- | creates the plot from input and scale | default set to 1 if not zoomed in or out
readAndDraw :: Element -> Double -> Canvas -> UI ()
readAndDraw input scale canvas =
  do s <- get value input

     set UI.fillStyle (UI.solidColor (UI.RGB 255 255 255)) (pure canvas)
     UI.fillRect (0,0) canWidth canHeight canvas
     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)

     case readExpr s of
      Just expr -> do
        plotExpr expr

      Nothing -> do
        return()

      where
        plotExpr expr1 = (id
          . drawLines
          . lines' expr1 scale) (canWidth, canHeight)

        --drawLines 
        drawLines [] = return ()
        drawLines ((p1, p2):xs) = do
            UI.beginPath canvas

            UI.moveTo p1 canvas
            UI.lineTo p2 canvas

            UI.closePath canvas
            UI.stroke canvas

            drawLines xs

-- | generates the differentiated function
ddt :: Element -> UI ()
ddt input = do
               fx <- get value input
               case readExpr fx of
                   Just expr -> do
                                  let dfxdt = differentiate  expr
                                  set value (show dfxdt) (pure input)
                                  return ()
                   Nothing -> do
                                return ()                           