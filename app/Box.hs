module Box where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle (degToRad)


{-----------------------------------------------------------
  This file defines the box in the simulation and the 
  functions used to move it.

------------------------------------------------------------}


data Box = Box {
  boxSize :: Float,
  boxEndPoints :: [Point],
  boxMass :: Float,
  boxVelocity :: Float
}

gravityAccel :: Float
gravityAccel = 9.81

-- Floor at y = 0
floorLevel :: Float
floorLevel = 0

constructBox :: Float -> Float -> Float -> Box
constructBox size mass offset =
    Box size [(offset, 0), (size + offset, 0), (size + offset, size), (offset, size)] mass 0

{-----------------------------------------------------------
  Returns a visual representation of the box.

------------------------------------------------------------}
drawBox :: Box -> Picture
drawBox (Box _ points _ _) = Color (makeColor 0.6 0.45 0.0 1) (Polygon points)

{-----------------------------------------------------------
  This function will make the box fall to the ground at constant acceleration.
  Once it reaches the ground, the velocity will reset back to zero.

------------------------------------------------------------}
gravity :: Float -> Box -> Box
gravity dt (Box size endPoints mass velocity)
  | all (\(_, y) -> y /= 0) endPoints = Box size newEndPoints mass (gravityAccel*dt + velocity)
  | otherwise = Box size endPoints mass 0
  where
    newEndPoints = [ (x, max (y - velocity*dt) 0) | (x, y) <- endPoints]


{-----------------------------------------------------------
  This function will move the arm in a specific direction when
  the arm is pushing it.

------------------------------------------------------------}
pushBox :: Float -> Point -> Box -> Box
pushBox pushDist (ex, ey) (Box size points mass velocity) =
    Box size newPoints mass velocity
  where
    -- Calculate the direction vector from the box's center to the end-effector
    (bx, by) = averagePoint points
    direction = normalize (bx - ex, by - ey) -- Reverse the direction for pushing motion

    -- Move the box in the direction of the push by a fixed distance
    (dx, dy) = scaleVector pushDist direction
    newPoints = translateBoxPts points dx dy

    -- Helpers
    normalize (x, y) = let len = max (sqrt (x * x + y * y)) 0.001 in (x / len, y / len)
    scaleVector scalar (x, y) = (scalar * x, scalar * y)

    averagePoint :: [Point] -> Point
    averagePoint pts = (avgX, avgY)
      where
        (sumX, sumY) = foldl (\(sx, sy) (x, y) -> (sx + x, sy + y)) (0, 0) pts
        n = fromIntegral (length pts)
        avgX = sumX / n
        avgY = sumY / n

{-----------------------------------------------------------
  This function is used to move the box such that it follows 
  the trajectory of the end effector.

------------------------------------------------------------}
moveGrippedBox :: Point -> Box -> Box
moveGrippedBox (dx, dy) (Box size endPoints mass velocity) =
  Box size (translateBoxPts endPoints dx dy) mass velocity

{-----------------------------------------------------------
  Translates all points of a box by dx and dy

------------------------------------------------------------}

translateBoxPts :: [Point] -> Float -> Float -> [Point]
translateBoxPts points dx dy = correctedPoints
  where
    -- Move all points by (dx, dy)
    newPoints = [ (x + dx, y + dy) | (x, y) <- points ]

    -- Calculate the lowest y-coordinate of the box (bottom edge)
    minY = minimum [y | (_, y) <- newPoints]

    -- Prevent the box from going below the floor (floorLevel = 0)
    floorCorrection = if minY < 0 then -minY else 0

    -- Apply the floor correction to all points
    correctedPoints = [ (x, y + floorCorrection) | (x, y) <- newPoints ]


rotateBoxPts :: [Point] -> Float -> [Point]
rotateBoxPts points angleDeg = [ rotate pt | pt <- points ]
  where
    angleRad = degToRad angleDeg
    rotate (x, y) = (x * (cos angleRad) - y * (sin angleRad), x * (sin angleRad) + y * (cos angleRad))


{-----------------------------------------------------------
  This function checks if a point is near or inside a boundary defined
  by a list of four points.

  The tolerance specifies how close the arm must be in order
  for this function to return true.

------------------------------------------------------------}
boundaryCheck :: Point -> Float -> [Point] -> Bool
boundaryCheck (x, y) tolerance points =
  let
    -- Extract the corners of the box
    minX = minimum [px | (px, _) <- points]
    maxX = maximum [px | (px, _) <- points]
    minY = minimum [py | (_, py) <- points]
    maxY = maximum [py | (_, py) <- points]

    condX = (minX - tolerance) <= x && x <= (maxX + tolerance)
    condY = (minY - tolerance) <= y && y <= (maxY + tolerance)
  in
    condX && condY
