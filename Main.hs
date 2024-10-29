import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate (animateIO, Controller (controllerSetRedraw))

{-  -}

window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)

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
