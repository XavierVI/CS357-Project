import Graphics.Gloss
import Arm ( RobotArm(RobotArm), Link(Link), drawArm, updateArm, updateGrabState )
import Box (Box(Box), drawBox, constructBox, moveGrippedBox, boundaryCheck, strictBoundaryCheck, pushBox, gravity)
import Graphics.Gloss.Interface.Pure.Game

data Sim = Sim { 
  robotArm :: RobotArm,
  simulationBox :: Box,
  pushBoxCondition :: Bool,
  pressedKeys :: [SpecialKey],
  gripBoxCondition :: Bool,
  prevEEPos :: Point
}


window :: Display
window = InWindow "Robot Arm Simulator" (900, 800) (50, 50)

initialArm :: RobotArm
initialArm = RobotArm [Link 100 78.27139, Link 100 (-73.63233)] (120, 106) False

initialBox :: Box
initialBox = constructBox 45 50 120


drawSim :: Sim -> Picture
drawSim (Sim arm box _ _ _ _) = Pictures [
    drawBox box, drawArm arm, floorPic,
    renderText "Controls: " (-300) 300,
    renderText "- Arrow keys to move the arm" (-300) 270,
    renderText "- Space bar to grip the box" (-300) 240,
    renderText "- Enter to reset the simulation" (-300) 210
  ]
  where
    -- base = translate 0 (-25) $ color (greyN 0.5) $ rectangleSolid 50 50
    floorPic = translate 0 (-200) $ color (greyN 0.5) $ rectangleSolid 900 400
    renderText t xPos yPos = Translate xPos yPos $ Scale 0.2 0.2 $ Text t


updateSim :: Float -> Sim -> Sim
updateSim dt (Sim arm box isPushed keys isGripped prevPos)
  | isGripped     = Sim newArm (moveGrippedBox diffInEEPos box) False keys isGripped eePos
  | pushCondition = Sim newArm (gravity dt $ pushBox pushDist eePos box) True keys isGripped eePos
  | otherwise     = Sim newArm (gravity dt box) (boundaryCheck eePos boxPoints) keys isGripped eePos
  where
    (xi, yi) = prevPos
    -- Update the arm and get the end-effector position
    (newArm, eePos) = updateArm dt (updateEEPositionFromKeys arm boxPoints keys)
    (xf, yf) = eePos
    diffInEEPos = (xf-xi, yf-yi)

    -- Extract box points for collision check
    Box _ boxPoints _ _ = box

    -- Fixed push distance
    pushDist = 5 -- Push distance
    pushCondition = not isPushed && boundaryCheck eePos boxPoints

{- 
  This function will return a new RobotArm with an updated position for the end effector
  based on the special keys that are pressed and if the resulting position
  is not inside the box.

 -}
updateEEPositionFromKeys :: RobotArm -> [Point] -> [SpecialKey] -> RobotArm
updateEEPositionFromKeys (RobotArm links (x,  y) grabState) boxPoints keys
  | strictBoundaryCheck (newX, newY) boxPoints = RobotArm links (x, y) grabState
  | otherwise = RobotArm links (newX, newY) grabState
  where
    newX
      | KeyRight `elem` keys = x+1
      | KeyLeft `elem` keys = x-1
      | otherwise = x

    newY
      | KeyUp `elem` keys = y+1
      | KeyDown `elem` keys = y-1
      | otherwise = y



inputHandler :: Event -> Sim -> Sim
inputHandler (EventKey (SpecialKey KeyEnter) Down _ _) _ =
    Sim initialArm initialBox False [] False (0, 0) -- Reset simulation to initial state

{- This input handler will check if the end effector is close to the box and
  is not already gripped. If this condition is true, it will set isGripped to True.
  Otherwise it will set it to False.

-}
inputHandler 
  (EventKey (SpecialKey KeySpace) Down _ _)
  (Sim arm box isPushed keys isGripped prevPos) =
    if boundaryCheck eePos points
    then Sim (updateGrabState arm) box isPushed keys (not isGripped) prevPos
    else Sim arm box isPushed keys False prevPos
      where
        Box _ points _ _ = box
        RobotArm _ eePos _ = arm

inputHandler
  (EventKey (SpecialKey key) Down _ _)
  (Sim arm box isPushed keys isGripped prevPos) =
    Sim arm box isPushed (key : keys) isGripped prevPos

inputHandler
  (EventKey (SpecialKey key) Up _ _)
  (Sim arm box isPushed keys isGripped prevPos) =
    Sim arm box isPushed (filter (/= key) keys) isGripped prevPos

inputHandler event sim = sim



main :: IO()
main = play
  window -- display
  (makeColor 0.8 0.8 0.8 1)  -- window color
  60     -- number of simulation steps to take each second
  (Sim initialArm initialBox False [] False (0,0)) -- initial state
  drawSim      -- function to draw the objects in the simulation
  inputHandler -- input handler for user input
  updateSim    -- function to call on each simulation step




