module Arm where


import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle
import Math

{- 

  This file is going to be where we define the model of the arm.
  Notes:
  - Our model (RobotArm) is defined as a list of Links, and each Link has a property length and angle.
  The angle is going to be relative to the previous link, or the x-axis for the first link.

 -}

-- Model of the arm (Links, joints)
-- data Arm = Arm [([Point], Float)]
data Link = Link Float Float
data RobotArm = RobotArm [Link]

-- Initial state functions
armLength :: Float
armLength = 100

initialState :: RobotArm
initialState = RobotArm [Link 100 90, Link 45 45]


{- 
 - This function takes a starting point and returns a Line that represents the link.

 -}
drawLink :: Link -> Point
drawLink (Link linkLength angle) = endPoint
  where
    x = linkLength * cos (degToRad angle)
    y = linkLength * sin (degToRad angle)
    endPoint = (x, y)

displayArm :: RobotArm -> Picture
displayArm (RobotArm links) = Color red $ Line ((0, 0) : [(x, y) | (x, y) <- points])
  where
    points = map drawLink links

