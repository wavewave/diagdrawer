module Main where

import System.Console.CmdArgs

import Application.DiagramDrawer.ProgType
import Application.DiagramDrawer.Command

main :: IO () 
main = do 
  putStrLn "diagdrawer"
  param <- cmdArgs mode

  commandLineProcess param