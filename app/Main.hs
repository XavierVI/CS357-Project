{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import Graphics.Gloss
import Arm ( RobotArm(RobotArm), Link(Link), drawArm, updateArm, updateArmGD )
import Box (Box(Box), drawBox, constructBox, updateBoxPosition, boundaryCheck)
import Graphics.Gloss.Interface.Pure.Game

data Sim = Sim RobotArm Box Bool

window :: Display
window = InWindow "Window" (800, 800) (50, 50)

initialArm :: RobotArm
initialArm = RobotArm [Link 100 90, Link 100 0] (120, 106)

initialBox :: Box
initialBox = constructBox 30 50 120

drawSim :: Sim -> Picture
drawSim (Sim arm box _) = Pictures [drawArm arm, drawBox box]

updateSim :: Float -> Sim -> Sim
updateSim dt (Sim arm box isPushed) =
    if not isPushed && boundaryCheck eePos boxPoints
    then Sim newArm (updateBoxPosition pushDist eePos box) True  -- Push once and set flag
    else Sim newArm box (boundaryCheck eePos boxPoints)  -- Reset flag when contact ends
  where
    -- Update the arm and get the end-effector position
    (newArm, eePos) = updateArm dt arm

    -- Extract box points for collision check
    Box _ boxPoints _ _ = box

    -- Fixed push distance
    pushDist = 5 -- Push distance

inputHandler :: Event -> Sim -> Sim
inputHandler (EventKey (SpecialKey KeySpace) Down _ _) _ =
    Sim initialArm initialBox False -- Reset simulation to initial state
inputHandler 
  (EventKey (SpecialKey KeyUp) Down _ _)
  (Sim (RobotArm links (x, y)) box isPushed) =
    Sim (RobotArm links (x, y+10)) box isPushed
inputHandler
  (EventKey (SpecialKey KeyDown) Down _ _)
  (Sim (RobotArm links (x, y)) box isPushed) =
    Sim (RobotArm links (x, y-10)) box isPushed
inputHandler
  (EventKey (SpecialKey KeyLeft) Down _ _)
  (Sim (RobotArm links (x, y)) box isPushed) =
    Sim (RobotArm links (x-10, y)) box isPushed
inputHandler 
  (EventKey (SpecialKey KeyRight) Down _ _)
  (Sim (RobotArm links (x, y)) box isPushed) =
    Sim (RobotArm links (x+10, y)) box isPushed
inputHandler event sim = sim

main :: IO()
main = play
  window
  white
  60
  (Sim initialArm initialBox False)
  drawSim
  inputHandler
  updateSim
