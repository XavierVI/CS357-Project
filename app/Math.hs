module Math where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle
  
{-
- This function will rotate a link by theta degrees.
- It's intended to be given the end point of a link
-}
rotateLine :: Float -> Point -> Point
rotateLine theta (x, y) = ( x*cos rad - y*sin rad, 
                            x*sin rad + y*cos rad )
  where
    rad = degToRad theta