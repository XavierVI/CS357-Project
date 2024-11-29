import Arm
import Graphics.Gloss

{- This file is only used to print out values for IK solvers using GHCI -}



testIK :: [Link] -> Point -> IO ()
testIK links target = print (ik links target)
