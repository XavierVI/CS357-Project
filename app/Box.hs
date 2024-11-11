module Box where

import Graphics.Gloss

{- 
  This file defines the box in the simulation and the functions used to move
  it.

 -}


-- Box   = [(x, y)] mass
-- the list contains the coordinates of each corner of the box
data Box = Box [Point] Float


{-
Returns the visual representation of the box

The box is drawn as a polygon

-}
drawBox :: Box -> Picture
drawBox (Box points mass) = Polygon points


-- updateBoxPosition


constructBox :: Float -> Float -> Box
constructBox size mass = (Box [(0,0), (size, 0), (size, size), (0, size)] mass)

