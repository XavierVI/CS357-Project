import Graphics.Gloss
import Arm ( RobotArm(RobotArm), Link(Link), drawArm, updateArm, updateArmGD )
import Box ( Box(Box), drawBox, constructBox, updateBoxPosition )
import Graphics.Gloss.Data.ViewPort ()

data Sim = Sim RobotArm Box

window :: Display
window = InWindow "Window" (800, 800) (50, 50)

arm :: RobotArm
arm = RobotArm [Link 100 90, Link 100 (-90)]

box :: Box
box = constructBox 30 50



drawSim :: RobotArm -> Box -> Picture
drawSim arm box = Pictures [drawArm arm, drawBox box]

updateSim :: ViewPort -> Float -> Sim -> Sim
updateSim _ dt (Sim RobotArm links Box size points mass) = undefined

main :: IO()
main = simulate
  window
  white
  60
  (Sim arm box)
  (drawSim arm box)
  updateSim

