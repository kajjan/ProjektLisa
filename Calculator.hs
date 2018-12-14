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


readAndDraw :: Element -> Double -> Canvas -> UI ()
readAndDraw input scale canvas =
  do s <- get value input

     set UI.fillStyle (UI.solidColor (UI.RGB 255 255 255)) (pure canvas)
     UI.fillRect (0,0) canvasWidth canvasHeight canvas
     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)

     case readExpr s of
      Just expr -> do
        plotExpr expr

      Nothing -> do
        return()

      where
        plotExpr expr1 = (id
          . drawLines
          . linez expr1 scale) (canvasWidth, canvasHeight)

        --drawLines 
        drawLines [] = return ()
        drawLines ((p1, p2):xs) = do
            UI.beginPath canvas

            UI.moveTo p1 canvas
            UI.lineTo p2 canvas

            UI.closePath canvas
            UI.stroke canvas

            drawLines xs