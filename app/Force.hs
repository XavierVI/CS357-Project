module Force where

import Graphics.Gloss.Data.Picture

type Velocity = (Float, Float)

data Model = Model {getVelocity :: Velocity, getPostition :: Point, getObject :: Object, getMass :: Float}

-- Sum type to hold the object in question currently just a box
data Object = Box Float Float


-- Convert object withn the Model into a picture currently just the Box but more rendering could be possible
objectToPicture :: Object -> Picture
objectToPicture (Box w l) = Polygon [(w,l),(-w,l),(-w,-l),(w,-l)]

initialVelocity :: Velocity
initialVelocity = (0,0)



initialBox :: Model
initialBox = Model initialVelocity (0,0) (Box 25 25) 50

-- Currently doesn't work because haskell doesn't like [Model]
--State :: [Model]
{-initalState :: State
initalState = [initialBox,adjustedBox]
    where
        adjustedBox = initialBox {getPostition = Position 100 -400 }-}

modelToPicture :: Model -> Picture
modelToPicture = toPicture
    where 
        toPicture model = 
            let
                x = fst $ getPostition model
                y = snd $ getPostition model
                o = getObject model
            in Translate x y $ objectToPicture o


-- Model -> Model function currently just applies gravity and velocity
modelToModel :: Model -> Model
modelToModel = next
    where
        next model = 
            let 
            (vx,vy) = getVelocity model
            (px,py) = getPostition model
            (nvx,nvy) = (vx, vy + g)
            (npx,npy) = (px + vx, py + vy)
            g = -1
            in Model {getVelocity = (nvx,nvy), getPostition = (npx,npy) , getObject = getObject model, getMass = getMass model } 
            --                                                            shouldn't have to do this, but it crashed without it
        
        