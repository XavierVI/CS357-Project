module Arm where


import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle
-- import Graphics.Gloss.Data.ViewPort (ViewPort)

{-----------------------------------------------------------------
  This file is going to be where we define the model of the arm and functions to draw/move the arm in the window.

------------------------------------------------------------------}

-- Model of the arm [Link length angle]
data Link = Link { length :: Float, jointAngle :: Float }
  deriving (Show)
data RobotArm = RobotArm { links :: [Link], eePos :: Point, grabState :: Bool }
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
drawArm (RobotArm links _ grabState) = Pictures [thickSegments, Pictures (map jointPicture points), gripper]
  where
    points = generatePoints links (0, 0)
    thickSegments = Pictures $ zipWith (drawThickSegment 10 black) points (tail points)
    jointPicture (x, y) = Translate x y (Color (makeColor 0.4 0.4 0.4 1) (ThickCircle 0 15))  -- Joints visualization
    lastPt = last points
    gripper = Pictures (drawGripper lastPt grabState)
    
drawGripper :: Point -> Bool -> [Picture]
drawGripper (x, y) grabState = [jointLink, gripperIndicator, threeDBar]
  where
    (x0, y0) = (x+7, y)
    (xf, yf) = (x0+4, y0)
    jointLink = drawThickSegment 4 black (x0, y0) (xf, yf)
    
    lightColor = if grabState then green else red
    gripperIndicator = drawThickSegment 4 lightColor (x0, y0+4) (xf, yf+4)
    
    (xu, yu) = (xf, yf)
    (xu', yu') = (xf+12, yf)
    threeDBar = drawThickSegment 10 (makeColor 0.1 0.1 0.1 1) (xu, yu) (xu', yu')

-- Draw a segment between two points
drawThickSegment :: Float -> Color -> Point -> Point -> Picture
drawThickSegment thickness c (x1, y1) (x2, y2) = Color c $ Polygon [p1, p2, p4, p3]
  where
    -- thickness = 10  -- Thickness of the arm
    dx = x2 - x1
    dy = y2 - y1
    len = sqrt (dx * dx + dy * dy)
    perpX = (dy / len) * (thickness / 2)  -- Perpendicular vector for width
    perpY = -(dx / len) * (thickness / 2)
    p1 = (x1 + perpX, y1 + perpY)
    p2 = (x1 - perpX, y1 - perpY)
    p3 = (x2 + perpX, y2 + perpY)
    p4 = (x2 - perpX, y2 - perpY)
    


-- updateArm :: ViewPort -> Float -> RobotArm -> RobotArm
updateArm :: Float -> RobotArm -> (RobotArm, Point)
updateArm dt (RobotArm links (xd, yd) grabState) =
  -- if the y-coodinate is below the ground, then return
  -- the current position
  if yd < 0 then (RobotArm links (xd, yd) grabState, fk links)
  else (RobotArm updatedLinks target grabState, fk updatedLinks)
  where
    target        = (xd, yd)
    desiredAngles = ik links target
    updatedLinks  = zipWith updateAngle links desiredAngles



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


-- returns the joint angles required for the desired position in radians
ik :: [Link] -> Point -> [Float]
ik links (x, y) = [radToDeg q1, radToDeg q2]
  where
    Link l1 a1 = head links
    Link l2 a2 = links !! 1
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

updateGrabState :: RobotArm -> RobotArm
updateGrabState arm = arm {grabState = not $ grabState arm}
