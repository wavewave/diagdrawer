module Application.DiagramDrawer.Command where

import Application.DiagramDrawer.ProgType
import Application.DiagramDrawer.Job

commandLineProcess :: Diagdrawer -> IO ()
commandLineProcess Test = do 
  putStrLn "test called"
  startJob
