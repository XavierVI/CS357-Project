module Arm where


import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle
-- import Graphics.Gloss.Data.ViewPort (ViewPort)

{-----------------------------------------------------------------
  This file is going to be where we define the model of the arm and
  functions to draw/move the arm in the window.

------------------------------------------------------------------}


data Link = Link { length :: Float, jointAngle :: Float }
  deriving (Show)
data RobotArm = RobotArm {
  armLinks :: [Link],
  eePos :: Point, 
  armGrabState :: Bool
}
  deriving (Show)


{---------------------------------------------------------------------
  This function generates the (x, y) coordinates of each link in cartesian space

-----------------------------------------------------------------------}
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
drawArm (RobotArm links _ grabState) = Pictures [thickSegments, Pictures (map jointPicture points), gripper]
  where
    points = generatePoints links (0, 0)
    thickSegments = Pictures $ zipWith (drawThickSegment 10 black) points (tail points)
    jointPicture (x, y) = Translate x y (Color (makeColor 0.4 0.4 0.4 1) (ThickCircle 0 15))  -- Joints visualization
    lastPt = last points
    gripper = Pictures (drawGripper lastPt grabState)


drawGripper :: Point -> Bool -> [Picture]
drawGripper (x, y) grabState = [gripperLink, grabStateIndicator, fingers]
  where
    -- draw the grab state indicator
    indicatorPos = 4
    indicatorSize = 4
    lightColor = if grabState then green else red
    grabStateIndicator = drawThickSegment 4 lightColor (x0, y0 + indicatorPos)
                                                       (xf, yf + indicatorSize)

    -- these points define the gripper link's position
    (x0, y0) = (x + 7, y)
    (xf, yf) = (x0 + 4, y0)
    
    -- these points define the position of the fingers
    (xu, yu) = (xf, yf)
    (xu', yu') = (xf + 12, yf)
    
    -- draw gripper link and fingers
    gripperLink = drawThickSegment 4 black (x0, y0) (xf, yf)
    fingers = drawThickSegment 10 (makeColor 0.1 0.1 0.1 1) (xu, yu) (xu', yu')


{---------------------------------------------------------------------
  Draw a segment between two points

-----------------------------------------------------------------------}
drawThickSegment :: Float -> Color -> Point -> Point -> Picture
drawThickSegment thickness c (x1, y1) (x2, y2) = Color c $ Polygon [p1, p2, p4, p3]
  where
    -- thickness = 10  -- Thickness of the arm
    dx = x2 - x1
    dy = y2 - y1
    len = sqrt (dx * dx + dy * dy)
    perpX = (dy / len) * (thickness / 2)  -- Perpendicular vector for width
    perpY = -((dx / len) * (thickness / 2))
    p1 = (x1 + perpX, y1 + perpY)
    p2 = (x1 - perpX, y1 - perpY)
    p3 = (x2 + perpX, y2 + perpY)
    p4 = (x2 - perpX, y2 - perpY)
    

{---------------------------------------------------------------------
  This function computes the required joint angles to move the arm
  to a desired position and nudges the arm towards that position.

-----------------------------------------------------------------------}
-- updateArm :: ViewPort -> Float -> RobotArm -> RobotArm
updateArm :: RobotArm -> (RobotArm, Point)
updateArm (RobotArm links (xd, yd) grabState) = 
  (RobotArm updatedLinks target grabState, fk updatedLinks)
  where
    target        = (xd, yd)
    desiredAngles = ik links target
    updatedLinks  = zipWith updateAngle links desiredAngles



{-------------------------------------------------------------
  Functions for moving the arm

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
ik links (x, y) = [radToDeg q1, radToDeg q2]
  where
    Link l1 _ = head links
    Link l2 _ = links !! 1
    gamma = atan2 y x
    distSquared = x**2 + y**2
    -- calculate joint angles
    q2 = - acos (clamp ((distSquared - l1**2 - l2**2) / (2*l1*l2)))
    s2 = l2 * sin q2
    c2 = l2 * cos q2
    q1 = gamma - atan (s2 / (l1 + c2))

-- Helper function to clamp values for acos
clamp :: Float -> Float
clamp val = max (-1) (min 1 val)


-- updates the angles for a link
updateAngle :: Link -> Float -> Link
updateAngle (Link l a) a'
  | abs (a' - a) > tolerance = Link l updatedAngle
  | otherwise                = Link l a
  where
    tolerance     = 0.5
    step          = 0.5
    updatedAngle  = a + signum (a' - a) * step


ikNewtonRaphson :: [Link] -> Point -> Int -> [Link]
ikNewtonRaphson links _ 0 = links
ikNewtonRaphson links p n = ikNewtonRaphson (newtonRaphson links p) p (n-1)

newtonRaphson :: [Link] -> Point -> [Link]
newtonRaphson links (xd, yd)
  | sqrt( e1**2 + e2**2 ) > tolerance = updatedLinks
  | otherwise = links
  where
    tolerance = 0.001
    Link l1 a1 = head links
    Link l2 a2 = links !! 1
    j        = jacobian links
    (x0, y0) = fk links
    (e1, e2) = (xd-x0, yd-y0)
    a1' = a1 + (head j * e1)
    a2' = a2 + (j !! 1 * e2)
    updatedLinks = [Link l1 a1', Link l2 a2']


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


