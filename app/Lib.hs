module Lib
    ( module Camera
    , module Interval
    , module Ray
    , module Vec3
    , module Random
    , degrees2radians
    ) where

import Camera
import Interval
import Random
import Ray
import Vec3


degrees2radians :: Double -> Double
degrees2radians deg = deg * pi / 180
