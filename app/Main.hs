import Graphics.Gloss
import Code
import Arm
import Force

main :: IO()
--main = display window white (displayArm (RobotArm [Link 100 90, Link 50 45, Link 100 90]))
main = simulate FullScreen white 24 initalState' stateToPicture' (update stateToState')

update f vp fn state = f state
    