import Graphics.Gloss
import Code
import Arm
import Gravity

main :: IO()
--main = display window white (displayArm (RobotArm [Link 100 90, Link 50 45, Link 100 90]))
main = simulate FullScreen white 24 initialObject objectToPicture (update objectToObject)

update f vp fn state = f state
    