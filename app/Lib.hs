module Lib
    ( infinity
    , degrees2radians
    , module Ray
    , module Vec3
    ) where

import Ray
import Vec3


infinity :: Double
infinity = 1 / 0


degrees2radians :: Double -> Double
degrees2radians deg = deg * pi / 180
