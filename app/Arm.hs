module Arm where


import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Math

{- 

  This file is going to be where we define the model of the arm.
  Notes:
  - the idea right now is the arm is going to be our model that has its own state (links and joint angles).
  - the links are defined as a [[Point]] and each end point is considered a joint (besides the very first [Point]).
  - What we want to do now, is write a function that can draw the arm in the desired orientation to the window. This is going to be a sequence of Lines, so we need to use Pictures to return a single Picture.

 -}

-- Model of the arm (Links, joints)
data Arm = Arm [([Point], Float)]


-- Initial state functions
armLength :: Float
armLength = 100

initialState :: [([Point], Float)]
initialState = [
  ([(0,   0), (200, 0)],    90),
  ([(200, 0), (400, 0)],     0)
  ]

initialJointAngles :: [Float]
initialJointAngles = [90, -30]  -- Initial angles in degrees


-- Functions to draw a link and joint
{-
  (x, y) = starting point
  length = length of the line segment

  This function only draws a simple line.
-}
drawLink :: ([Point], Float) -> Picture
drawLink ([(x0, y0), (x1, y1)], angle) = Line [(x0, y0), rotateLine angle (x1, y1)]

displayArm :: Picture
displayArm = Pictures (map drawLink initialState)

   