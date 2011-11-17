{-# LANGUAGE DeriveDataTypeable #-}

module Application.DiagramDrawer.Type where 

import System.Console.CmdArgs

data Diagdrawer = Test 
              deriving (Show,Data,Typeable)

test :: Diagdrawer
test = Test 

mode = modes [test]

