module Force where

import Graphics.Gloss.Data.Picture


-- this does work i was doing something wrong
type State = [Model]

data Model = 
    Model {getVelocity :: (Float, Float), getPostition :: Point, getObject :: Object, getMass :: Float, getForces :: [(Float,Float)]}
        deriving (Show, Eq)
-- Sum type to hold the object in question currently just a box
data Object = Box Float Float
    deriving (Show, Eq)


applyForces :: Model -> (Float, Float)
applyForces m = 
    let
    mass      = getMass m
    (vx,vy)   = getVelocity m
    (nfx,nfy) = foldr (\(x1,y1) (x2,y2) -> (x1+x2,y1+y2) ) (0,0) (getForces m)
    (ax,ay)   = (nfx/mass ,nfy/mass) 
    in (vx + ax,vy + ay)


-- Convert object withn the Model into a picture currently just the Box but more rendering could be possible
objectToPicture :: Object -> Picture
objectToPicture (Box w l) = Polygon [(w,l),(-w,l),(-w,-l),(w,-l)]

initialVelocity :: (Float, Float)
initialVelocity = (10,10)

initialBox :: Model
initialBox = Model initialVelocity (0,0) (Box 25 25) 50 [(0,-50)]

--TODO: State functions are marked with ' as to not conflict with other files will need to be changed or merged later
initalState' :: State
initalState' = [initialBox,adjustedBox]
    where
        adjustedBox = initialBox {getPostition = (100,400) }

stateToPicture' :: State -> Picture
stateToPicture' state = Pictures (map toPicture state)
    where 
        toPicture model = 
            let
                x = fst $ getPostition model
                y = snd $ getPostition model
                o = getObject model
            in Translate x y $ objectToPicture o


-- Model -> Model function currently just applies gravity and velocity
stateToState' :: State -> State
stateToState'  = map next
    where
        next model = 
            let 
            (vx,vy) = getVelocity model
            (px,py) = getPostition model
            (nvx,nvy) = applyForces model
            (npx,npy) = (px + vx, py + vy)
            in model {getVelocity = (nvx,nvy), getPostition = (npx,npy) } 
            -- Model == new decleration | model == remake old one with changes