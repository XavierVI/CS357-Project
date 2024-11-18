module Box (Box(..), drawBox, constructBox, updateBoxPosition, boundaryCheck) where

import Graphics.Gloss
-- import Graphics.Gloss.Data.ViewPort (ViewPort)
import Graphics.Gloss.Geometry.Angle (degToRad)

{- 
  This file defines the box in the simulation and the functions used to move it.

 -}


-- Box   = [(x, y)] mass
-- the list contains the coordinates of each corner of the box
data Box = Box Float [Point] Float Point

constructBox :: Float -> Float -> Float -> Box
constructBox size mass offset =
    Box size [(offset, 0), (size + offset, 0), (size + offset, size), (offset, size)] mass (0, 0)

{-
Returns the visual representation of the box

The box is drawn as a polygon

-}
drawBox :: Box -> Picture
drawBox (Box _ points _ _) = Color (makeColorI 170 20 255 255) (Polygon points)

updateBoxPosition :: Float -> Point -> Box -> Box
updateBoxPosition pushDist (ex, ey) (Box size points mass velocity) =
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
boundaryCheck (x, y) points = cond1 && cond2
  where
    (x1, y1) = head points
    (x2, y2) = points !! 1
    (x3, y3) = points !! 2
    (x4, y4) = points !! 3
    cond1 = (x1 <= x && x <= x2) && (x4 <= x && x <= x3)
    cond2 = (y1 <= y && y <= y3) && (y2 <= y && y <= y4)




