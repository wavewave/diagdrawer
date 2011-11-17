{-# LANGUAGE DeriveDataTypeable #-}

module Application.DiagramDrawer.ProgType where 

import System.Console.CmdArgs

data Diagdrawer = Test 
              deriving (Show,Data,Typeable)

test :: Diagdrawer
test = Test 

mode = modes [test]

