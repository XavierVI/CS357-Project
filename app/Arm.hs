module Arm where


import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.ViewPort (ViewPort)

{- 
  This file is going to be where we define the model of the arm and functions to draw/move the arm in the window.

 -}

-- Model of the arm (Links, joints)
data Link = Link Float Float
  deriving (Show)
data RobotArm = RobotArm [Link]
  deriving (Show)


{- 
  - These functions are used to display the arm in the window

  - drawLink takes a Link and a starting point and returns the end point for the link
  
  - generatePoints takes a list of links and a point which represents the base of the arm. It generates a list of points which represent the location of each joint
  
  - displayArm takes a RobotArm and returns a Line, which is a visual representation of the arm.

  ## Moving the arm in the window:
  The function updateArm is going to update the joint angles for the links and automatically
  call the previous three functions to redraw the arm

 -}
drawLink :: Link -> Point -> Point
drawLink (Link linkLength angle) (x, y) = endPoint
  where
    x' = linkLength * cos (degToRad angle)
    y' = linkLength * sin (degToRad angle)
    endPoint = (x+x', y+y')


generatePoints :: [Link] -> Point -> [Point]
generatePoints [] _ = []
generatePoints (link:links) prevPoint = endPoint : generatePoints links endPoint
  where
    endPoint = drawLink link prevPoint


drawArm :: RobotArm -> Picture
drawArm (RobotArm links) = Line ((0, 0) : [(x, y) | (x, y) <- points])
  where
    points = generatePoints links (0,0)



-- analytical IK stuff
updateArm :: ViewPort -> Float -> RobotArm -> RobotArm
updateArm _ dt (RobotArm links) = RobotArm updatedLinks
  where
    step          = (5.0 * pi) / 180
    desiredAngles = ik links (200, 0)
    updatedLinks  = zipWith (\(Link l a) a' -> Link l (a + signum (a' - a) * step)) links desiredAngles


ik :: [Link] -> Point -> [Float]
ik [Link l1 a1, Link l2 a2] (x, y) = [a1', a2']
  where
    -- Link l1 a1 = links !! 0
    -- Link l2 a2 = links !! 1
    a2' = acos ( (x**2 + y**2 - l1**2 - l2**2) / (2*l1*l2) )
    a1' = atan (y/x) - atan ((l2*sin a2') / (l1 + l2 * cos a2')  )
ik links (x, y) = [0.0, 0.0]

-- GD stuff
-- redraw the arm with a new angle for each joint
-- for now, lets assume we only have two links
updateArmGD :: ViewPort -> Float -> RobotArm -> RobotArm
updateArmGD _ dt (RobotArm links) = RobotArm updatedLinks
  where
    learningRate = 0.001
    target       = (-150, 0)
    gradient     = gradLoss [links !! 0, links !! 1] target
    updatedLinks = zipWith updateLinkAngle [links !! 0, links !! 1] gradient
    updateLinkAngle (Link len angle) grad = Link len (angle - learningRate * grad)
    -- a1' = a1 - learningRate * (gradient !! 0)
    -- a2' = a2 - learningRate * (gradient !! 1)
{- updateArm _ dt (RobotArm [(Link l1 a1), (Link l2 a2)]) = RobotArm [(Link l1 (a1+dt)), (Link l2 (a2+dt))]
 -}
loss :: [Link] -> Point -> Float
loss [Link l1 theta1, Link l2 theta2] (xd, yd) = sqrt ( term1**2 + term2**2 )
  where
    term1 = xd - l1 * cos theta1 + l2 * cos theta2
    term2 = yd - l1 * sin theta1 + l2 * sin theta2


-- assume its only two joints, otherwise it's hard to compute the partial derivative
-- with respect to other parameters
gradLoss :: [Link] -> Point -> [Float]
gradLoss [Link l1 a1, Link l2 a2] dPoint = [pLossWRTa1, pLossWRTa2]
  where
    h          = 0.0001
    loss1      = loss [Link l1 a1,     Link l2 a2    ] dPoint
    adjLoss1   = loss [Link l1 (a1+h), Link l2 a2    ] dPoint
    loss2      = loss [Link l1 a1,     Link l2 a2    ] dPoint
    adjLoss2   = loss [Link l1 a1,     Link l2 (a2+h)] dPoint
    pLossWRTa1 = (adjLoss1 - loss1) / h
    pLossWRTa2 = (adjLoss2 - loss2) / h
-- gradLoss links dPoint = undefined


