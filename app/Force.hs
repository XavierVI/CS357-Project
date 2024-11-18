module Force where

import Graphics.Gloss.Data.Picture

data Position = Position {px :: Float, py :: Float}
    deriving (Show)

data Velocity = Velocity {vx :: Float, vy :: Float}
    deriving (Show)

data Model = Model {getVelocity :: Velocity, getPostition :: Position, getObject :: Object, getMass :: Float}
    deriving (Show)

-- Sum type to hold the object in question currently just a box
data Object = Box Float Float
    deriving (Show)

-- Convert object withn the Model into a picture currently just the Box but more rendering could be possible
objectToPicture :: Object -> Picture
objectToPicture (Box w l) = Polygon [(w,l),(-w,l),(-w,-l),(w,-l)]

initialVelocity :: Velocity
initialVelocity = Velocity 0 0

initialPosition :: Position
initialPosition = Position 0 0

initialBox :: Model
initialBox = Model initialVelocity initialPosition (Box 25 25) 50

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
                x = px $ getPostition model
                y = py $ getPostition model
                o = getObject model
            in Translate x y $ objectToPicture o


-- Model -> Model function currently just applies gravity and velocity
modelToModel :: Model -> Model
modelToModel = next
    where
        next model = 
            let 
            v = getVelocity model
            p = getPostition model
            nv = v {vx = vx v, vy = vy v - g}
            np = p {px = px p + vx v, py = py p + vy v}
            g = 1
            in Model {getVelocity = nv, getPostition = np, getObject = getObject model, getMass = getMass model } 
            --                                               shouldn't have to do this, but it crashed without it
        
        