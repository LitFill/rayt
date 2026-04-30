module Lib
    ( degrees2radians
    , module Interval
    , module Ray
    , module Vec3
    , module Camera
    ) where

import Camera
import Interval
import Ray
import Vec3


degrees2radians :: Double -> Double
degrees2radians deg = deg * pi / 180
