{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import Graphics.Gloss
import Arm ( RobotArm(RobotArm), Link(Link), drawArm, updateArm, updateArmGD )
import Box ( Box(Box), drawBox, constructBox, updateBoxPosition )
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Graphics.Gloss.Interface.Pure.Game


data Sim = Sim RobotArm Box

window :: Display
window = InWindow "Window" (800, 800) (50, 50)

arm :: RobotArm
arm = RobotArm [Link 100 90, Link 100 0] (120, 106)

box :: Box
box = constructBox 30 50



drawSim :: Sim -> Picture
drawSim (Sim arm box) = Pictures [drawArm arm, drawBox box]

-- You have to remove the view port for the 'play' function
-- updateSim :: ViewPort -> Float -> Sim -> Sim
updateSim :: Float -> Sim -> Sim
updateSim dt (Sim arm box) = Sim (updateArm dt arm) (updateBoxPosition dt box)

inputHandler :: Event -> Sim -> Sim
inputHandler 
  (EventKey (SpecialKey KeyUp) Down _ _)
  (Sim (RobotArm links (x, y)) box) =
    Sim (RobotArm links (x, y+10)) box
inputHandler
  (EventKey (SpecialKey KeyDown) Down _ _)
  (Sim (RobotArm links (x, y)) box) =
    Sim (RobotArm links (x, y-10)) box
inputHandler
  (EventKey (SpecialKey KeyLeft) Down _ _)
  (Sim (RobotArm links (x, y)) box) =
    Sim (RobotArm links (x-10, y)) box
inputHandler 
  (EventKey (SpecialKey KeyRight) Down _ _)
  (Sim (RobotArm links (x, y)) box) =
    Sim (RobotArm links (x+10, y)) box
inputHandler event sim = sim


main :: IO()
main = play
  window
  white
  60
  (Sim arm box)
  drawSim
  inputHandler
  updateSim
  

-- main = simulate
--   window
--   white
--   60
--   (Sim arm box)
--   drawSim
--   updateSim





