module Application.DiagramDrawer.Iteratee where

import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.State
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.IO.Class

import Application.DiagramDrawer.Type
import Application.DiagramDrawer.Draw

iter :: Iteratee DiagramEvent DiagramStateIO () 
iter = do liftIO (putStrLn "I am waiting first result")
          sequence_ (repeat eventProcess)

eventProcess :: Iteratee DiagramEvent DiagramStateIO ()
eventProcess = do 
  r1 <- await 
  case r1 of 
    ButtonClear -> do 
      dstate <- lift get
      let n = count dstate 
      liftIO $ putStrLn $ "button pressed " ++ show (n+1) ++ " times"
      lift . put $ dstate {count = n+1}
    CanvasClick (x,y) -> do 
      dstate <- lift get 
      let c = canvas dstate
          cm = crossmark dstate
      liftIO $ putStrLn $ "canvas pressed at " ++ show (x,y)
      liftIO $ redrawCrossmarkAfterErase c cm (x,y)
      lift . put $ dstate { crossmark = Just (x,y) } 

