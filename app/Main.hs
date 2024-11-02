import Graphics.Gloss
import Code
import Arm

main :: IO()
main = display window white (displayArm (RobotArm [Link 100 45, Link 45 90]))
  
    