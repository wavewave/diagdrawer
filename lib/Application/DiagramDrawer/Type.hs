module Application.DiagramDrawer.Type where

import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Data.Functor.Identity (Identity(..))
import Control.Monad.State

import Data.IORef

type Trampoline m x = Coroutine Identity m x 
type Generator a m x = Coroutine (Yield a) m x
type Iteratee a m x = Coroutine (Await a) m x

data DiagramState = 
  DiagramState 
  { count :: Int 
  }

type DiagramStateIO = StateT DiagramState IO

data DiagramEvent = ButtonClear
                  deriving (Show,Eq,Ord)

emptyDiagramState :: DiagramState
emptyDiagramState = DiagramState { count = 0 }