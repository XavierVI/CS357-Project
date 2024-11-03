import Graphics.Gloss
import Code
import Arm

main :: IO()
main = display window white (displayArm (RobotArm [Link 100 90, Link 50 45, Link 100 90]))
  
    