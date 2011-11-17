module Application.DiagramDrawer.GUI where

import Graphics.UI.Gtk 

startGUI :: IO () 
startGUI = do 
  initGUI 
  window <- windowNew 
  hbox <- hBoxNew False 0 
  vbox <- vBoxNew False 0 
  canvas <- drawingAreaNew 
  buttonClear <- buttonNewWithLabel "Clear"
  buttonLine  <- buttonNewWithLabel "Line"
  buttonArc   <- buttonNewWithLabel "Arc"
  buttonPoint <- buttonNewWithLabel "Point"
  set window [containerChild := vbox] 
  boxPackStart hbox buttonClear PackGrow 0 
  boxPackStart hbox buttonLine  PackGrow 0 
  boxPackStart hbox buttonArc   PackGrow 0 
  boxPackStart hbox buttonPoint PackGrow 0 
  boxPackStart vbox canvas      PackGrow 0 
  boxPackStart vbox hbox        PackNatural 0 
  canvas `on` sizeRequest $ return (Requisition 400 300)
  widgetShowAll window 
  onDestroy window mainQuit 
  mainGUI
  return () 
