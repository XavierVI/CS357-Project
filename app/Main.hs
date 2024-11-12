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
inputHandler event sim = sim
-- inputHandler (EventKey (SpecialKey KeyUp) Down _ _) sim = sim

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





