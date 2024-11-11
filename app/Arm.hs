module Arm where


import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.ViewPort (ViewPort)

{-----------------------------------------------------------------
  This file is going to be where we define the model of the arm and functions to draw/move the arm in the window.

------------------------------------------------------------------}

-- Model of the arm [Link length angle]
data Link = Link Float Float
  deriving (Show)
data RobotArm = RobotArm [Link]
  deriving (Show)


{---------------------------------------------------------------------
  - These functions are used to display the arm in the window

  - drawLink takes a Link and a starting point and returns the end point for the link
  
  - generatePoints takes a list of links and a point which represents the base of the arm. It generates a list of points which represent the location of each joint
  
  - displayArm takes a RobotArm and returns a Line, which is a visual representation of the arm.

  ## Moving the arm in the window:
  The function updateArm is going to update the joint angles for the links and automatically
  call the previous three functions to redraw the arm

-----------------------------------------------------------------------}

-- this function generates the position of each link in cartesian space
generatePoints :: [Link] -> Point -> [Point]
generatePoints [] _ = []
generatePoints [Link l1 a1, Link l2 a2] _ = [(0,0), p1, p2]
  where
    p1 = drawLink (Link l1 a1) (0,0)
    p2 = drawLink (Link l2 (a1+a2)) p1
generatePoints (link:links) prevPoint = endPoint : generatePoints links endPoint
  where
    endPoint = drawLink link prevPoint
    
drawLink :: Link -> Point -> Point
drawLink (Link linkLength angle) (x, y) = endPoint
  where
    x' = linkLength * cos (degToRad angle)
    y' = linkLength * sin (degToRad angle)
    endPoint = (x+x', y+y')

drawArm :: RobotArm -> Picture
drawArm (RobotArm links) = Line ((0, 0) : [(x, y) | (x, y) <- points])
  where
    points = generatePoints links (0,0)

updateArm :: ViewPort -> Float -> RobotArm -> RobotArm
updateArm _ dt (RobotArm links) = RobotArm updatedLinks
  where
    currAngles    = [ a | Link _ a <- links ]
    desiredAngles = [ a | Link _ a <- ikNewtonRaphson links (0, 200) 500]
    updatedLinks = zipWith updateAngle links desiredAngles


-- redraws the arm with a new angle for each joint using gradient descent
updateArmGD :: ViewPort -> Float -> RobotArm -> RobotArm
updateArmGD _ dt (RobotArm links)
  | abs(loss target links) > tolerance = RobotArm updatedLinks
  | otherwise = RobotArm links
  where
    tolerance = 0.5
    learningRate = 0.09
    target       = (0, 200)
    grad         = gradient (loss target) links
    updatedLinks = zipWith updateLinkAngle links grad
    updateLinkAngle (Link len angle) g = Link len (angle - learningRate * g)


{-------------------------------------------------------------
  Math functions

--------------------------------------------------------------}

-- returns the x-cooridinate of the end-effector
position :: (Float -> Float) -> [Link] -> Float
position _ []                   = 0 
position f [Link len a]         = len * f (degToRad a)
position f ((Link len a):links) = p + position f newLinks
  where
    Link tailLen tailA = head links
    newLinks = Link tailLen (tailA+a) : tail links
    p = len * f (degToRad a)

-- returns the end point of the end-effector
fk :: [Link] -> Point
fk []    = (0,0)
fk links = (x, y)
  where
    x = position cos links
    y = position sin links

ikNewtonRaphson :: [Link] -> Point -> Int -> [Link]
ikNewtonRaphson links _ 0 = links
ikNewtonRaphson links p n = ikNewtonRaphson (newtonRaphson links p) p (n-1)

-- returns the joint angles required for the desired position in radians
ik :: [Link] -> Point -> [Float]
ik links (x, y) = [a1', a2']
  where
    Link l1 a1 = head links
    Link l2 a2 = links !! 1
    gamma = atan2 y x
    alpha = acos ( clamp ((x**2 + y**2 + l1**2 - l2**2) / (2*l1*sqrt(x**2 + y**2)) ))
    beta  = acos ( clamp ((x**2 + y**2 - l1**2 - l2**2) / 2*(l1*l2)))
    a1' = gamma - alpha
    a2' = pi - beta

-- Helper function to clamp values for acos
clamp :: Float -> Float
clamp val = max (-1) (min 1 val)


-- updates the angles for a link, a' should be passed in radians
updateAngle :: Link -> Float -> Link
updateAngle (Link l a) a'
  | abs (a' - degToRad a) > tolerance = Link l (radToDeg updatedAngle)
  | otherwise               = Link l a
  where
    tolerance     = (5.0 * pi) / 180
    step          = (1.0 * pi) / 180
    updatedAngle  = degToRad a + signum (a' - degToRad a) * step



loss :: Point -> [Link] -> Float
loss (xd, yd) links = sqrt ( (term1**2) + (term2**2) )
  where
    term1 = xd - position cos links
    term2 = yd - position sin links


{- 
  The gradient takes a scalar valued function and returns a vector of the
  partial derivative of the function over each parameter.

  These parameters are determined by the point we are evaluating each partial
  derivative at.

 -}
gradient :: ([Link] -> Float) -> [Link] -> [Float]
gradient f links = [partial1, partial2]
  where
    Link l1 a1 = head links
    Link l2 a2 = links !! 1
    h          = 0.0001
    t1         = f [Link l1 a1,     Link l2 a2    ]
    t1'        = f [Link l1 (a1+h), Link l2 a2    ]
    t2         = f [Link l1 a1,     Link l2 a2    ]
    t2'        = f [Link l1 a1,     Link l2 (a2+h)]
    partial1   = (t1' - t1) / h
    partial2   = (t2' - t2) / h


jacobian :: [Link] -> [Float]
jacobian links = [a1', a2']
  where
    a1' = sum (gradient (position cos) links)
    a2' = sum (gradient (position sin) links)


newtonRaphson :: [Link] -> Point -> [Link]
newtonRaphson links (xd, yd)
  | sqrt( e1**2 + e2**2 ) > tolerance = updatedLinks
  | otherwise = links
  where
    tolerance = 10.0
    Link l1 a1 = head links
    Link l2 a2 = links !! 1
    j   = jacobian links
    (x0, y0) = fk links
    (e1, e2) = (xd-x0, yd-y0)
    a1' = a1 + (head j * e1)
    a2' = a2 + (j !! 1 * e2)
    updatedLinks = [Link l1 a1', Link l2 a2']



