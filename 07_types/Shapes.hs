module Shapes
( Point
, Shape
, area
, nudge
, baseCircle
, baseRect
) where

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rect Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ radius) = pi * radius ^ 2
area (Rect (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) radius) a b = (Circle (Point (x+a) (y+b)) radius)
nudge (Rect (Point x1 y1) (Point x2 y2)) a b = Rect (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRect :: Float -> Float -> Shape
baseRect width height= Rect (Point 0 0) (Point height width)


