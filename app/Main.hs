import Graphics.Gloss
import Arm ( RobotArm(RobotArm), Link(Link), drawArm, updateArm, updateGrabState )
import Box (Box(Box), drawBox, constructBox, moveGrippedBox, boundaryCheck, pushBox, gravity)
import Graphics.Gloss.Interface.Pure.Game

data Sim = Sim { 
  robotArm :: RobotArm,
  simulationBox :: Box,
  pushBoxCondition :: Bool,
  pressedKeys :: [SpecialKey],
  gripBoxCondition :: Bool,
  -- for tracking the trajectory the box should travel when gripped
  prevEEPos :: Point
}


window :: Display
window = InWindow "Robot Arm Simulator" (900, 800) (50, 50)

windowColor :: Color
windowColor = makeColor 0.8 0.8 0.8 1

initialArmState :: RobotArm
initialArmState = RobotArm [Link 100 78.27139, Link 100 (-73.63233)] (120, 106) False

initialBoxState :: Box
initialBoxState = constructBox 45 50 120

initialSimState :: Sim
initialSimState = Sim initialArmState initialBoxState False [] False (0,0)


drawSim :: Sim -> Picture
drawSim (Sim arm box _ _ _ _) = Pictures [
    drawBox box, drawArm arm, floorPicture,
    renderText "Controls: " (-300) 300,
    renderText "- Arrow keys to move the arm" (-300) 270,
    renderText "- Space bar to grip the box" (-300) 240,
    renderText "- Enter to reset the simulation" (-300) 210
  ]
  where
    -- base = translate 0 (-25) $ color (greyN 0.5) $ rectangleSolid 50 50
    floorPicture = translate 0 (-200) $ color (greyN 0.5) $ rectangleSolid 900 400
    renderText t xPos yPos = Translate xPos yPos $ Scale 0.2 0.2 $ Text t

{-----------------------------------------------------------
  This is the main function for changing the state of the
  simulation.

  If the arm has gripped the box, then the box will follow
  the trajectory of the arm until it is released.

  If the box is not gripped but the end effector's new position
  is inside the box, then the box will be pushed a specific distance.

  Otherwise, the arm's position will change, and the box will remain
  stationary of fall if it is not touching the ground.

------------------------------------------------------------}
updateSim :: Float -> Sim -> Sim
updateSim dt (Sim arm box isPushed keys isGripped prevPos)
  | isGripped     = Sim updatedArm movedBox False keys isGripped eePos
  | pushCondition = Sim updatedArm (gravity dt $ pushBox pushDist eePos box) True keys isGripped eePos
  | otherwise     = Sim updatedArm (gravity dt box) False keys isGripped eePos
  where
    -- boundary tolerance allowed for the pushing the box
    pushTolerance = 5

    -- Update the arm and get the end-effector position
    (updatedArm, eePos) = updateArm (updateEEPositionFromKeys arm boxPoints keys)
    boxTrajectory = (fst eePos - fst prevPos, snd eePos - snd prevPos)

    -- Move the box only if gripped
    movedBox = if isGripped then moveGrippedBox boxTrajectory box else box

    -- Extract box points for use in checks
    Box _ boxPoints _ _ = box

     -- Fixed push distance
    pushDist = 5 -- Push distance
    pushCondition = not isPushed && boundaryCheck eePos pushTolerance boxPoints


{-----------------------------------------------------------
  This function will return a RobotArm with an updated position
  for the end effector based on the special keys that are pressed.

  It will not update the position of the end effector if the new
  position is too far inside the box or into the ground.

------------------------------------------------------------}
updateEEPositionFromKeys :: RobotArm -> [Point] -> [SpecialKey] -> RobotArm
updateEEPositionFromKeys (RobotArm links (x, y) grabState) boxPoints keys
  | grabState = RobotArm links (newX, newY) grabState  -- Relaxed constraints while gripping
  | boundaryCheck (newX, newY) tolerance boxPoints = RobotArm links (x, y) grabState
  | otherwise = RobotArm links (newX, newY) grabState
  where
    tolerance = 1
    newX
      | KeyRight `elem` keys = x + 1
      | KeyLeft `elem` keys = x - 1
      | otherwise = x

    newY
      | KeyUp `elem` keys = y + 1
      | KeyDown `elem` keys && y - 1 >= 0 = y - 1
      | otherwise = y



inputHandler :: Event -> Sim -> Sim

{-----------------------------------------------------------
  Reset the simulation to the initial state

------------------------------------------------------------}
inputHandler (EventKey (SpecialKey KeyEnter) Down _ _) _ =
    Sim initialArmState initialBoxState False [] False (0, 0)

{-----------------------------------------------------------
  This input handler will check if the arm is close enough to grip
  the box and then will change the grip state of the arm.

------------------------------------------------------------}
inputHandler 
  (EventKey (SpecialKey KeySpace) Down _ _)
  (Sim arm box isPushed keys isGripped prevPos) =
    if boundaryCheck eePos tolerance points
    then Sim (updateGrabState arm) box isPushed keys (not isGripped) prevPos
    else Sim arm box isPushed keys False prevPos
      where
        tolerance = 15
        Box _ points _ _ = box
        RobotArm _ eePos _ = arm

{-----------------------------------------------------------
  These inputHandlers will add/remove the arrow keys to the special keys
  list of the simulation model.

  Once a key is pressed down, it's added to the list until it
  is released.

------------------------------------------------------------}
inputHandler
  (EventKey (SpecialKey key) Down _ _)
  (Sim arm box isPushed keys isGripped prevPos) =
    Sim arm box isPushed (key : keys) isGripped prevPos

inputHandler
  (EventKey (SpecialKey key) Up _ _)
  (Sim arm box isPushed keys isGripped prevPos) =
    Sim arm box isPushed (filter (/= key) keys) isGripped prevPos

inputHandler _ sim = sim

main :: IO()
main = play
  window -- display
  windowColor  -- window color
  60     -- number of simulation steps each second
  initialSimState
  drawSim      -- function to draw the objects in the simulation
  inputHandler -- input handler for user input
  updateSim    -- function to call on each simulation step