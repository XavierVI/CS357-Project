module Box (Box(..), drawBox, constructBox, updateBoxPosition, boundaryCheck) where

import Graphics.Gloss
-- import Graphics.Gloss.Data.ViewPort (ViewPort)
import Graphics.Gloss.Geometry.Angle (degToRad)
import Debug.Trace (traceShow)

{- 
  This file defines the box in the simulation and the functions used to move it.

 -}


-- Box   = [(x, y)] mass
-- the list contains the coordinates of each corner of the box
data Box = Box Float [Point] Float Point

-- Floor at y = 0
floorLevel :: Float
floorLevel = 0

constructBox :: Float -> Float -> Float -> Box
constructBox size mass offset =
    Box size [(offset, 0), (size + offset, 0), (size + offset, size), (offset, size)] mass (0, 0)

{-
Returns the visual representation of the box

The box is drawn as a polygon

-}
drawBox :: Box -> Picture
drawBox (Box _ points _ _) = Color (makeColorI 170 20 255 255) (Polygon points)

<<<<<<< Updated upstream
updateBoxPosition :: Float -> Point -> Box -> Box
updateBoxPosition pushDist (ex, ey) (Box size points mass velocity) =
=======

{- 
  This function will make the box fall to the ground at constant acceleration.
  Once it reaches the ground, the velocity will reset back to zero.
 -}
gravity :: Float -> Box -> Box
gravity dt (Box size endPoints mass velocity)
  | all (\(_, y) -> y /= 0) endPoints = Box size newEndPoints mass (gravityAccel*dt + velocity)
  | otherwise = Box size endPoints mass 0
  where
    newEndPoints = [ (x, max (y - velocity*dt) 0) | (x, y) <- endPoints]

{- 
  This function is used to move the arm when the end effector pushes it.
 -}
pushBox :: Float -> Point -> Box -> Box
pushBox pushDist (ex, ey) (Box size points mass velocity) =
>>>>>>> Stashed changes
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

<<<<<<< Updated upstream
=======
{- 
  This function is used to move the box such that it follows the end effector of the arm.
 -}
moveGrippedBox :: Point -> Box -> Box
moveGrippedBox (dx, dy) (Box size endPoints mass velocity) = Box size correctedPoints mass velocity
  where
    -- Move all points by (dx, dy)
    newPoints = [ (x + dx, y + dy) | (x, y) <- endPoints ]

    -- Calculate the lowest y-coordinate of the box
    minY = minimum [y | (_, y) <- newPoints]

    -- Prevent the box from going below the floor
    floorCorrection = if minY < 0 then -minY else 0

    -- Apply the floor correction to all points
    correctedPoints = [ (x, y + floorCorrection) | (x, y) <- newPoints ]

>>>>>>> Stashed changes
-- Translates all points of a box by dx and dy
translateBoxPts :: [Point] -> Float -> Float -> [Point]
translateBoxPts points dx dy = [ (x + dx, y + dy) | (x, y) <- points ]

rotateBoxPts :: [Point] -> Float -> [Point]
rotateBoxPts points angleDeg = [ rotate pt | pt <- points ]
  where
    angleRad = degToRad angleDeg
    rotate (x, y) = (x * (cos angleRad) - y * (sin angleRad), x * (sin angleRad) + y * (cos angleRad))


-- checks if a point is touching or inside the box
boundaryCheck :: Point -> [Point] -> Bool
<<<<<<< Updated upstream
boundaryCheck (x, y) points = cond1 && cond2
  where
    (x1, y1) = head points
    (x2, y2) = points !! 1
    (x3, y3) = points !! 2
    (x4, y4) = points !! 3
    cond1 = (x1 <= x && x <= x2) && (x4 <= x && x <= x3)
    cond2 = (y1 <= y && y <= y3) && (y2 <= y && y <= y4)



=======
boundaryCheck (x, y) points =
  let
    tolerance = 5 -- Tolerance for boundary checks
    -- Extract the corners of the box
    minX = minimum [px | (px, _) <- points]
    maxX = maximum [px | (px, _) <- points]
    minY = minimum [py | (_, py) <- points]
    maxY = maximum [py | (_, py) <- points]

    condX = (minX - tolerance) <= x && x <= (maxX + tolerance)
    condY = (minY - tolerance) <= y && y <= (maxY + tolerance)
  in
    condX && condY

strictBoundaryCheck :: Point -> [Point] -> Bool
strictBoundaryCheck (x, y) points =
  let
    tolerance = 1 -- Tolerance for strict boundary checks
    (x1, y1) = head points     -- Bottom-left corner
    (x2, _) = points !! 1      -- Bottom-right corner
    (_, y4) = points !! 3      -- Top-left corner
>>>>>>> Stashed changes

    condX = (x1 - tolerance) <= x && x <= (x2 + tolerance) -- Check if X is within bounds
    condY = (y1 - tolerance) <= y && y <= (y4 + tolerance) -- Check if Y is within bounds
  in condX && condY
