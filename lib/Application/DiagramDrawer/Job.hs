module Application.DiagramDrawer.Job where

import Application.DiagramDrawer.GUI

startJob :: IO () 
startJob = do 
  putStrLn "job started"
  startGUI 


