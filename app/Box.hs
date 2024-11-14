module Box where

import Graphics.Gloss
-- import Graphics.Gloss.Data.ViewPort (ViewPort)
import Graphics.Gloss.Geometry.Angle (degToRad)

{- 
  This file defines the box in the simulation and the functions used to move it.

 -}


-- Box   = [(x, y)] mass
-- the list contains the coordinates of each corner of the box
data Box = Box Float [Point] Float

constructBox :: Float -> Float -> Float -> Box
constructBox size mass offset =
  Box size [(offset,0), (size+offset, 0), (size+offset, size), (offset, size)] mass

{-
Returns the visual representation of the box

The box is drawn as a polygon

-}
drawBox :: Box -> Picture
drawBox (Box size points mass) = Polygon points

-- updateBoxPosition :: ViewPort -> Float -> Box -> Box
updateBoxPosition :: Float -> Point -> Box -> Box
updateBoxPosition dt eePos (Box size points mass) = 
  if boundaryCheck eePos points
  then Box size newPoints mass
  else Box size points mass
  where
    -- instead of dt, we can use some force measurement
    newPoints = translateBoxPts points dt 0


translateBoxPts :: [Point] -> Float -> Float -> [Point]
translateBoxPts points dx dy = [ (x+dx, y+dy) | (x,y) <- points]


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




