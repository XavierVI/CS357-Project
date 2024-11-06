{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Arm where


import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle
import Math
import Graphics.Gloss.Data.ViewPort (ViewPort)

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

-- redraw the arm with a new angle for each joint
-- for now, lets assume we only have two links
updateArm :: ViewPort -> Float -> RobotArm -> RobotArm
updateArm _ dt (RobotArm [(Link l1 a1), (Link l2 a2)]) = RobotArm [(Link l1 t1), (Link l2 t2)]
  where
    [t1, t2] = ik [(Link l1 a1), (Link l2 a2)] (200, 200)



loss :: [Link] -> Point -> Float
loss [Link l1 theta1, Link l2 theta2] (xd, yd) = sqrt ( term1**2 + term2**2 )
  where
    term1 = xd - l1 * cos theta1 + l2 * cos theta2
    term2 = yd - l1 * sin theta1 + l2 * sin theta2

lossSlope :: [Link] -> Point -> [Float]
lossSlope links desiredPoint = [ diff slope | Link len slope <- links ]
  where
    updatedLinks = [ Link l t+0.00001 | (Link l t) <- links ]
    diff updatedLink = (loss updatedLinks desiredPoint - loss links desiredPoint) / 0.00001


gradDesc :: [Link] -> Point -> [Link]
gradDesc links desiredPoint = [ go newLink loss | newLink <- zip links losses ]
  where
    learningRate = 0.01
    losses = loss links desiredPoint
    go (Link l theta) loss = theta - learningRate * loss
