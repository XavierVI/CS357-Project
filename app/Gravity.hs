module Gravity where

import Graphics.Gloss.Data.Picture
--                vx     vy      x     y     w       l     m
type Object = [(Float, Float, Float, Float, Float, Float, Float)]

{-type Velocity = (Float, Float)
--type Position = (Float, Float)

initialVelocity :: Velocity
initialVelocity = (0,0)

initialPosition :: Position
initialPosition = (0,0)
-}

box :: Float -> Float -> Path
box w l = [(w,l),(-w,l),(-w,-l),(w,-l)]

initialObject :: Object
initialObject = [(0,0,-400,-100,25,50,50)]


objectToPicture :: Object -> Picture
objectToPicture state = Pictures (map toPicture state)
    where toPicture (_,_,x,y,w,l,m) = Translate x y $ Polygon $ box w l



objectToObject :: Object -> Object
objectToObject = map next 
  where 
    next (vx,vy,x,y,w,l,m)  = (nvx, nvy, x + vx, ny, w, l, m)
      where
        nvx = vx     -- * 0.98 -- Basic air resistance type thing
        nvy = vy - g
        ny = y + nvy
        g = 1  


