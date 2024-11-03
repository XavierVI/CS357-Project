module Arm where


import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle
import Math

{- 
  This file is going to be where we define the model of the arm and functions to draw/move the arm in the window.

 -}

-- Model of the arm (Links, joints)
data Link = Link Float Float
data RobotArm = RobotArm [Link]


{- 
  - These functions are used to display the arm in the window

  - drawLink takes a Link and a starting point and returns the end point for the link
  
  - generatePoints takes a list of links and a point which represents the base of the arm. It generates a list of points which represent the location of each joint
  
  - displayArm takes a RobotArm and returns a Line, which is a visual representation of the arm.

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


displayArm :: RobotArm -> Picture
displayArm (RobotArm links) = Line ((0, 0) : [(x, y) | (x, y) <- points])
  where
    points = generatePoints links (0,0)


