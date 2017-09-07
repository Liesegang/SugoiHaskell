module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidVolume
, cuboidArea
) where

sphereVolume :: Float -> Float
sphereVolume r = 4.0 / 3.0 * pi * r ^ 3

sphereArea :: Float -> Float
sphereArea r = 4 * pi * r ^ 2

cubeVolume :: Float -> Float
cubeVolume side = cuboidVolume side side side

cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = reactArea a b * c

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = 2 * reactArea a b + 2 * reactArea b c + 2 * reactArea c a

reactArea :: Float -> Float -> Float
reactArea a b = a * b
