{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import Graphics.Gloss
import Arm ( RobotArm(RobotArm), Link(Link), drawArm, updateArm )
import Box (Box(Box), drawBox, constructBox, updateBoxPosition, boundaryCheck)
import Graphics.Gloss.Interface.Pure.Game

data Sim = Sim RobotArm Box Bool [SpecialKey]


window :: Display
window = InWindow "Window" (900, 800) (50, 50)

initialArm :: RobotArm
initialArm = RobotArm [Link 100 78.27139, Link 100 (-73.63233)] (120, 106)

initialBox :: Box
initialBox = constructBox 30 50 120


drawSim :: Sim -> Picture
drawSim (Sim arm box _ _) = Pictures [drawArm arm, drawBox box, floorPic]
  where
    -- base = translate 0 (-25) $ color (greyN 0.5) $ rectangleSolid 50 50
    floorPic = translate 0 (-25) $ color (greyN 0.5) $ rectangleSolid 800 50


updateSim :: Float -> Sim -> Sim
updateSim dt (Sim arm box isPushed keys) =
    if not isPushed && boundaryCheck eePos boxPoints
    then Sim newArm (updateBoxPosition pushDist eePos box) True keys -- Push once and set flag
    else Sim newArm box (boundaryCheck eePos boxPoints) keys  -- Reset flag when contact ends
  where
    -- Update the arm and get the end-effector position
    (newArm, eePos) = updateArm dt (updateArmPositionsFromKeys arm keys)

    -- Extract box points for collision check
    Box _ boxPoints _ _ = box

    -- Fixed push distance
    pushDist = 5 -- Push distance

updateArmPositionsFromKeys :: RobotArm -> [SpecialKey] -> RobotArm
updateArmPositionsFromKeys (RobotArm links (x,  y)) keys = RobotArm links (newX, newY)
  where
    newX
      | KeyRight `elem` keys = x+1
      | KeyLeft `elem` keys = x-1
      | otherwise = x

    newY
      | KeyUp `elem` keys = y+1
      | KeyDown `elem` keys = y-1
      | otherwise = y



inputHandler :: Event -> Sim -> Sim
inputHandler (EventKey (SpecialKey KeySpace) Down _ _) _ =
    Sim initialArm initialBox False [] -- Reset simulation to initial state

inputHandler
  (EventKey (SpecialKey key) Down _ _)
  (Sim arm box isPushed keys) =
    Sim arm box isPushed (key : keys)

inputHandler
  (EventKey (SpecialKey key) Up _ _)
  (Sim arm box isPushed keys) =
    Sim arm box isPushed (filter (/= key) keys)

inputHandler event sim = sim



main :: IO()
main = play
  window -- display
  (makeColorI 220 220 220 10)  -- window color
  60     -- number of simulation steps to take each second
  (Sim initialArm initialBox False []) -- initial state
  drawSim      -- function to draw the objects in the simulation
  inputHandler -- input handler for user input
  updateSim    -- function to call on each simulation step




