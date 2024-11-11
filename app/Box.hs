module Box where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)

{- 
  This file defines the box in the simulation and the functions used to move
  it.

 -}


-- Box   = [(x, y)] mass
-- the list contains the coordinates of each corner of the box
data Box = Box Float [Point] Float

constructBox :: Float -> Float -> Box
constructBox size mass = (Box size [(0,0), (size, 0), (size, size), (0, size)] mass)

{-
Returns the visual representation of the box

The box is drawn as a polygon

-}
drawBox :: Box -> Picture
drawBox (Box size points mass) = Polygon points

updateBoxPosition :: ViewPort -> Float -> Box -> Box
updateBoxPosition _ dt (Box size points mass) = Box size (rotateBoxPts points (pi/4)) mass

translateBoxPts :: [Point] -> Float -> Float -> [Point]
translateBoxPts points dx dy = [ (x+dx, y+dy) | (x,y) <- points]


rotateBoxPts :: [Point] -> Float -> [Point]
rotateBoxPts points theta = [ rotate pt | pt <- points ]
  where
    rotate (x, y) = (x * (cos theta) - y * (sin theta), x * (sin theta) + y * (cos theta))


