import Graphics.Gloss
import Arm ( RobotArm(RobotArm), Link(Link), drawArm, updateArm, updateArmGD )
import Box ( Box(Box), drawBox, constructBox, updateBoxPosition )
import Graphics.Gloss.Data.ViewPort ()

window :: Display
window = InWindow "Window" (800, 800) (50, 50)

main :: IO()
-- main = display window white (drawBox (constructBox 50 50))
main = simulate
  window
  white
  60
  (constructBox 100 50)
  drawBox
  updateBoxPosition

-- main = simulate
--   window
--   white
--   60
--   (RobotArm [Link 100 90, Link 100 (-90)])
--   drawArm
--   updateArm


{- 
  Examples
 
box :: Float -> Float -> Path
box x y = [(u, w) | u <- [0..x], w <- [0..y]]

{- This can only form a diagonal path -}
linePath :: Float -> Float -> Path
linePath x y = [(u, w) | (u, w) <- zip [0..x] [0..y]]



-- animating a growing circle
animationFunc :: Float -> Picture
animationFunc time = Circle (2*time)
-- mainAnimatedCircle = animate window white animationFunc

-- pendulum example: https://mmhaskell.com/blog/2019/3/25/making-a-glossy-game-part-1
type Model = (Float, Float)

simulationRate :: Int
simulationRate = 20

initialModel :: Model
initialModel = (0,0)

drawingFunc :: Model -> Picture
drawingFunc (theta, dtheta) = Line [(0, 0), (200 * cos theta, 200 * sin theta)]

updateFunc :: ViewPort -> Float -> Model -> Model
updateFunc _ dt (theta, dtheta) = (theta + dt * dtheta, dtheta - dt * (cos theta))
    
pendulumMain = simulate
  window
  white
  simulationRate
  initialModel
  drawingFunc
  updateFunc
  -}