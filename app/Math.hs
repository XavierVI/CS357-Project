module Math where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle
  

-- GD Loss function
-- loss :: [Link] -> Float -> Float -> Float
-- loss [Link l1 theta1, Link l2 theta2] xd yd = sqrt( term1**2 + term2**2 )
--   where
--     term1 = xd - l1 * cos theta1 + l2 * cos theta2
--     term2 = xd - l1 * sin theta1 + l2 * sin theta2
