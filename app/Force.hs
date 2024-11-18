module Force where

import Graphics.Gloss.Data.Picture

data Position = Position {px :: Float, py :: Float}
    deriving (Show)

data Velocity = Velocity {vx :: Float, vy :: Float}
    deriving (Show)

data Model = Model {getVelocity :: Velocity, getPostition :: Position, getObject :: Object, getMass :: Float}
    deriving (Show)

data Object = Box Float Float
    deriving (Show)

--data State :: [Model]

objectToPicture :: Object -> Path
objectToPicture (Box w l) = [(w,l),(-w,l),(-w,-l),(w,-l)]

initialVelocity :: Velocity
initialVelocity = Velocity 0 0

initialPosition :: Position
initialPosition = Position 0 0

initialBox :: Model
initialBox = Model initialVelocity initialPosition (Box 25 25) 50

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
            in Translate x y (Polygon $ objectToPicture o)


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
        
        

{-


objectToObject :: Object -> Object
objectToObject = map next 
  where 
    next (vx,vy,x,y,w,l,m)  = (nvx, nvy, x + vx, ny, w, l, m)
      where
        nvx = vx     -- * 0.98 -- Basic air resistance type thing
        nvy = vy - g
        ny = y + nvy
        g = 1  


-}