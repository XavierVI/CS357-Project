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
    desiredAngles = ik links  (-60, 128.06248)
    -- updatedLinks  = zipWith updateAngle links desiredAngles
    updatedLinks = newtonRaphson links (0, 200)

-- redraw the arm with a new angle for each joint
-- for now, lets assume we only have two links
updateArmGD :: ViewPort -> Float -> RobotArm -> RobotArm
updateArmGD _ dt (RobotArm links) = RobotArm updatedLinks
  where
    learningRate = 0.01
    target       = (200, 0)
    gradient     = gradLoss [links !! 0, links !! 1] target
    updatedLinks = zipWith updateLinkAngle [links !! 0, links !! 1] gradient
    updateLinkAngle (Link len angle) grad = Link len (angle - learningRate * grad)
    -- a1' = a1 - learningRate * (gradient !! 0)
    -- a2' = a2 - learningRate * (gradient !! 1)
{- updateArm _ dt (RobotArm [(Link l1 a1), (Link l2 a2)]) = RobotArm [(Link l1 (a1+dt)), (Link l2 (a2+dt))]
 -}


{-------------------------------------------------------------
  Mathematical functions

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

-- returns the joint angles required for the desired position in radians
ik :: [Link] -> Point -> [Float]
ik links (x, y) = [a1', a2']
  where
    Link l1 a1 = head links
    Link l2 a2 = links !! 1
    gamma = atan2 y x
    alpha = acos ( clamp ((x**2 + y**2 + l1**2 - l2**2) / (2*l1*sqrt(x**2 + y**2)) ))
    beta  = acos ( clamp ((x**2 + y**2 - l1**2 - l2**2) / 2*(l1*l2)))
    a1' = gamma + alpha
    a2' = beta - pi

-- Helper function to clamp values for acos
clamp :: Float -> Float
clamp val = max (-1) (min 1 val)

-- updates the angles for a link, a' should be passed in radians
updateAngle :: Link -> Float -> Link
updateAngle (Link l a) a'
  | abs (a' - a) > tolerance = Link l (radToDeg updatedAngle)
  | otherwise               = Link l a
  where
    tolerance     = (5.0 * pi) / 180
    step          = (1.0 * pi) / 180
    updatedAngle  = degToRad a + signum (a' - degToRad a) * step


loss :: [Link] -> Point -> Float
loss links (xd, yd) = sqrt ( term1**2 + term2**2 )
  where
    Link l1 theta1 = head links
    Link l2 theta2 = links !! 1
    term1 = xd - l1 * cos (degToRad theta1) + l2 * cos (degToRad theta2)
    term2 = yd - l1 * sin (degToRad theta1) + l2 * sin (degToRad theta2)


-- assume its only two joints, otherwise it's hard to compute the partial derivative
-- with respect to other parameters
gradLoss :: [Link] -> Point -> [Float]
gradLoss links dPoint = [pLossWRTa1, pLossWRTa2]
  where
    Link l1 a1 = head links
    Link l2 a2 = links !! 1
    h          = 0.1
    loss1      = loss [Link l1 a1,     Link l2 a2    ] dPoint
    adjLoss1   = loss [Link l1 (a1+h), Link l2 a2    ] dPoint
    loss2      = loss [Link l1 a1,     Link l2 a2    ] dPoint
    adjLoss2   = loss [Link l1 a1,     Link l2 (a2+h)] dPoint
    pLossWRTa1 = (adjLoss1 - loss1) / h
    pLossWRTa2 = (adjLoss2 - loss2) / h

{- 
  The gradient takes a scalar valued function and returns a vector of the
  partial derivative of the function over each parameter.

  These parameters are determined by the point we are evaluating each partial
  derivative at.

 -}

-- 
gradient :: ([Link] -> Float) -> [Link] -> [Float]
gradient f links = [partial1, partial2]
  where
    Link l1 a1 = head links
    Link l2 a2 = links !! 1
    h          = 0.001
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



