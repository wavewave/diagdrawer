module Application.DiagramDrawer.Draw where

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import Data.IORef 

import Application.DiagramDrawer.Type

redrawCrossmarkAfterErase :: DrawingArea 
                          -> Maybe (Double,Double) 
                          -> (Double,Double)
                          -> IO ()
redrawCrossmarkAfterErase canvas cm (x,y) = do 
  win <- widgetGetDrawWindow canvas 
  renderWithDrawable win $ do 
    case cm of 
      Just (x0,y0) -> eraseCrossmark (x0,y0) 
      Nothing -> return ()
    drawCrossmark (x,y)

eraseCrossmark :: (Double,Double) -> Render ()
eraseCrossmark (x,y) = do 
  rectangle (x-10) (y-10) 20 20
  --fill 
  stroke 

drawCrossmark :: (Double,Double) -> Render ()
drawCrossmark (x,y) = do 
  moveTo (x-10) y 
  lineTo (x+10) y
  moveTo x (y-10)
  lineTo x (y+10)
  stroke

