module Application.DiagramDrawer.GUI where

import Graphics.UI.Gtk 
import Control.Monad.IO.Class
import Control.Monad.State 
import Control.Monad.Coroutine
import Data.IORef

import Application.DiagramDrawer.Type 
import Application.DiagramDrawer.Coroutine
import Application.DiagramDrawer.Iteratee 

startGUI :: IO () 
startGUI = do 
  initGUI 
  window <- windowNew 
  hbox <- hBoxNew False 0 
  vbox <- vBoxNew False 0 
  cvs <- drawingAreaNew 
  buttonClear <- buttonNewWithLabel "Clear"
  buttonLine  <- buttonNewWithLabel "Line"
  buttonArc   <- buttonNewWithLabel "Arc"
  buttonPoint <- buttonNewWithLabel "Point"
  set window [containerChild := vbox] 
  boxPackStart hbox buttonClear PackGrow 0 
  boxPackStart hbox buttonLine  PackGrow 0 
  boxPackStart hbox buttonArc   PackGrow 0 
  boxPackStart hbox buttonPoint PackGrow 0 
  boxPackStart vbox cvs      PackGrow 0 
  boxPackStart vbox hbox        PackNatural 0 
  cvs `on` sizeRequest $ return (Requisition 400 300)

  let st = emptyDiagramState { canvas = cvs }
  (r,st') <- runStateT (resume iter) st
  sref <- newIORef st'
  tref <- case r of 
            Left aw -> do 
              newIORef aw
            Right _ -> error "what?"

  buttonClear `on` buttonPressEvent $ tryEvent $ do 
    liftIO $ bouncecallback tref sref ButtonClear
 
  cvs `on` buttonPressEvent $ tryEvent $ do 
    (x,y) <- eventCoordinates 
    liftIO $ bouncecallback tref sref (CanvasClick (x,y))
 

 {-
  canvas `on` motionNotifyEvent $ tryEvent $ do 
    (x,y) <- eventCoordinates
    liftIO $ putStrLn $ show (x,y)
  -}

  {-
  canvas `on` buttonPressEvent $ tryEvent $ do 
    (x,y) <- eventCoordinates 
    liftIO $ putStrLn $ "button pressed at " ++ show (x,y)
  -}

  widgetAddEvents cvs [PointerMotionMask,Button1MotionMask] 

  widgetShowAll window 
  onDestroy window mainQuit 
  mainGUI
  return () 

