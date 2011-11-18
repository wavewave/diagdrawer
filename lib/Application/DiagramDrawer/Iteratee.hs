module Application.DiagramDrawer.Iteratee where

import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.State
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.IO.Class

import Application.DiagramDrawer.Type

iter :: Iteratee DiagramEvent DiagramStateIO () 
iter = do liftIO (putStrLn "I am waiting first result")
          await
          await 
          await 
          return () 