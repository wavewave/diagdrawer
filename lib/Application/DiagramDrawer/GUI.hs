{-# LANGUAGE QuasiQuotes #-}

module Application.DiagramDrawer.GUI where

import Application.DiagramDrawer.Verbatim

import Graphics.UI.Gtk 
import Control.Monad.IO.Class
import Control.Monad.State 
import Control.Monad.Coroutine
import Data.IORef

import Application.DiagramDrawer.Type 
import Application.DiagramDrawer.Coroutine
import Application.DiagramDrawer.Iteratee 

uiDecl :: String 
uiDecl = [verbatim|<ui>
  <menubar> 
    <menu action="FMA">
      <menuitem action="SELECTA" />
      <menuitem action="SELECTB" />
    </menu>
  </menubar>
</ui>
|]

selectmod :: [RadioActionEntry]
selectmod = [ RadioActionEntry "SELECTA" "Select A" Nothing Nothing Nothing 0
            , RadioActionEntry "SELECTB" "Select B" Nothing Nothing Nothing 1 
            ] 

startGUI :: IO () 
startGUI = do 
  initGUI 
  window <- windowNew
  ui <- uiManagerNew 
  uiManagerAddUiFromString ui uiDecl
  fma <- actionNew "FMA" "File" Nothing Nothing 
  -- selecta <- actionNew "SELECTA" "Select A" Nothing Nothing
  -- selectb <- actionNew "SELECTB" "Select B" Nothing  Nothing

  agr <- actionGroupNew "AGR"
  mapM_ (actionGroupAddAction agr) [fma] -- ,selecta,selectb]
  actionGroupAddRadioActions agr selectmod 0 (const (return ()))
  Just ra <- actionGroupGetAction agr "SELECTA"
  set (castToRadioAction ra) [radioActionCurrentValue := 0]  



  uiManagerInsertActionGroup ui agr 0

  maybeMenubar <- uiManagerGetWidget ui "/ui/menubar"
  let menubar = case maybeMenubar of 
                  Just x -> x 
                  Nothing -> error "cannot get menubar from string"
 
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
  boxPackStart vbox canvas      PackGrow 0 
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

