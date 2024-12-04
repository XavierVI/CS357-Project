{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import Graphics.Gloss
import Arm ( RobotArm(RobotArm), Link(Link), drawArm, updateArm, updateArmGD )
import Box (Box(Box), drawBox, constructBox, updateBoxPosition, boundaryCheck)
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace (traceShow)

data Sim = Sim RobotArm Box Bool

window :: Display
window = InWindow "Window" (800, 800) (50, 50)

initialArm :: RobotArm
initialArm = RobotArm [Link 100 90, Link 100 0] (120, 106)

initialBox :: Box
initialBox = constructBox 30 50 120

drawSim :: Sim -> Picture
drawSim (Sim arm box _) = Pictures [drawArm arm, drawBox box]

updateSim :: Float -> Sim -> Sim
<<<<<<< Updated upstream
updateSim dt (Sim arm box isPushed) =
    if not isPushed && boundaryCheck eePos boxPoints
    then Sim newArm (updateBoxPosition pushDist eePos box) True  -- Push once and set flag
    else Sim newArm box (boundaryCheck eePos boxPoints)  -- Reset flag when contact ends
  where
    -- Update the arm and get the end-effector position
    (newArm, eePos) = updateArm dt arm
=======
updateSim dt (Sim arm box isPushed keys isGripped prevPos)
  | isGripped = Sim constrainedArm constrainedBox False keys isGripped eePos
  | otherwise = Sim newArm (gravity dt box) False keys isGripped eePos
  where
    -- Update the arm and get the position
    (newArm, eePos) = updateArm dt (updateEEPositionFromKeys arm boxPoints keys)
    diffInEEPos = (fst eePos - fst prevPos, snd eePos - snd prevPos)
>>>>>>> Stashed changes

    -- Move the box based on arm movement
    Box size boxPoints mass velocity = box
    movedBox = moveGrippedBox diffInEEPos box

<<<<<<< Updated upstream
    -- Fixed push distance
    pushDist = 5 -- Push distance
=======
    -- Constrain the box by the floor
    Box _ correctedPoints _ _ = movedBox
    minY = minimum [y | (_, y) <- correctedPoints]
    floorCorrection = if minY < 0 then -minY else 0
    constrainedBox = if floorCorrection /= 0
                     then Box size [(x, y + floorCorrection) | (x, y) <- correctedPoints] mass velocity
                     else movedBox

    -- Prevent the arm from moving into the box if the box is constrained
    constrainedArm =
      if floorCorrection /= 0 && isColliding arm constrainedBox diffInEEPos
      then arm -- Stop the arm's movement
      else newArm

    -- Check if the arm is colliding with the box
    isColliding :: RobotArm -> Box -> Point -> Bool
    isColliding (RobotArm _ eePos _) (Box _ points _ _) (dx, dy) =
      boundaryCheck eePos points && dy < 0 -- Only restrict downward motion

{- 
  This function will return a new RobotArm with an updated position for the end effector
  based on the special keys that are pressed and if the resulting position
  is not inside the box.

 -}
updateEEPositionFromKeys :: RobotArm -> [Point] -> [SpecialKey] -> RobotArm
updateEEPositionFromKeys (RobotArm links (x, y) grabState) boxPoints keys
  | grabState = RobotArm links (newX, newY) grabState  -- Relaxed constraints while gripping
  | strictBoundaryCheck (newX, newY) boxPoints = RobotArm links (x, y) grabState
  | otherwise = RobotArm links (newX, newY) grabState
  where
    newX
      | KeyRight `elem` keys = x + 1
      | KeyLeft `elem` keys = x - 1
      | otherwise = x

    newY
      | KeyUp `elem` keys = y + 1
      | KeyDown `elem` keys = y - 1
      | otherwise = y
>>>>>>> Stashed changes

inputHandler :: Event -> Sim -> Sim
inputHandler (EventKey (SpecialKey KeySpace) Down _ _) _ =
    Sim initialArm initialBox False -- Reset simulation to initial state
inputHandler 
  (EventKey (SpecialKey KeyUp) Down _ _)
  (Sim (RobotArm links (x, y)) box isPushed) =
    Sim (RobotArm links (x, y+10)) box isPushed
inputHandler
  (EventKey (SpecialKey KeyDown) Down _ _)
  (Sim (RobotArm links (x, y)) box isPushed) =
    Sim (RobotArm links (x, y-10)) box isPushed
inputHandler
  (EventKey (SpecialKey KeyLeft) Down _ _)
  (Sim (RobotArm links (x, y)) box isPushed) =
    Sim (RobotArm links (x-10, y)) box isPushed
inputHandler 
  (EventKey (SpecialKey KeyRight) Down _ _)
  (Sim (RobotArm links (x, y)) box isPushed) =
    Sim (RobotArm links (x+10, y)) box isPushed
inputHandler event sim = sim

main :: IO()
main = play
<<<<<<< Updated upstream
  window
  white
  60
  (Sim initialArm initialBox False)
  drawSim
  inputHandler
  updateSim
=======
  window -- display
  (makeColor 0.8 0.8 0.8 1)  -- window color
  60     -- number of simulation steps to take each second
  (Sim initialArm initialBox False [] False (0,0)) -- initial state
  drawSim      -- function to draw the objects in the simulation
  inputHandler -- input handler for user input
  updateSim    -- function to call on each simulation step
>>>>>>> Stashed changes
