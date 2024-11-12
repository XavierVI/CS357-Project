module Box where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Graphics.Gloss.Geometry.Angle (degToRad)

{- 
  This file defines the box in the simulation and the functions used to move
  it.

 -}


-- Box   = [(x, y)] mass
-- the list contains the coordinates of each corner of the box
data Box = Box Float [Point] Float

constructBox :: Float -> Float -> Box
constructBox size = 
  Box size [(0,0), (size, 0), (size, size), (0, size)]

{-
Returns the visual representation of the box

The box is drawn as a polygon

-}
drawBox :: Box -> Picture
drawBox (Box size points mass) = Polygon points

-- updateBoxPosition :: ViewPort -> Float -> Box -> Box
updateBoxPosition :: Float -> Box -> Box
updateBoxPosition dt (Box size points mass) = Box size (rotateBoxPts (translateBoxPts points dt dt) dt) mass

translateBoxPts :: [Point] -> Float -> Float -> [Point]
translateBoxPts points dx dy = [ (x+dx, y+dy) | (x,y) <- points]


rotateBoxPts :: [Point] -> Float -> [Point]
rotateBoxPts points angleDeg = [ rotate pt | pt <- points ]
  where
    angleRad = degToRad angleDeg
    rotate (x, y) = (x * (cos angleRad) - y * (sin angleRad), x * (sin angleRad) + y * (cos angleRad))


