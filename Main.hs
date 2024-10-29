import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)

{-  -}

window :: Display
window = InWindow "Nice Window" (800, 800) (50, 50)

box :: Float -> Float -> Path
box x y = [(u, w) | u <- [0..x], w <- [0..y]]

{- This can only form a diagonal path -}
linePath :: Float -> Float -> Path
linePath x y = [(u, w) | (u, w) <- zip [0..x] [0..y]]


{- 
Notes:
- the display function takes a display, color, and picture as input and returns an IO. It opens a new window and displays the given picture.

- Displays describe how Gloss should display its output (what the window should look like)
- 

 -}
main = display window white (Line (linePath 80 80))

-- animating a growing circle
animationFunc :: Float -> Picture
animationFunc time = Circle (2*time)

mainAnimatedCircle = animate window white animationFunc


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

pendulumExample = simulate
  window
  white
  simulationRate
  initialModel
  drawingFunc
  updateFunc
    