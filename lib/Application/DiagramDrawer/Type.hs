module Application.DiagramDrawer.Type where

import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Data.Functor.Identity (Identity(..))
import Control.Monad.State

import Graphics.UI.Gtk
import Data.IORef

type Trampoline m x = Coroutine Identity m x 
type Generator a m x = Coroutine (Yield a) m x
type Iteratee a m x = Coroutine (Await a) m x

data DiagramState = 
  DiagramState 
  { count :: Int 
  , canvas :: DrawingArea
  , crossmark :: Maybe (Double, Double)
  }

type DiagramStateIO = StateT DiagramState IO

data DiagramEvent = ButtonClear
                  | CanvasClick (Double,Double)
                  deriving (Show,Eq,Ord)

emptyDiagramState :: DiagramState
emptyDiagramState = DiagramState { count = 0, canvas = undefined, crossmark = Nothing }