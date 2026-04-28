{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}

module Ray where

import Data.Function ((&))

import Vec3


data Ray = Ray
    {origin, direction :: !Vec3}
    deriving (Show)


rayAt :: Double -> Ray -> Vec3
rayAt t (Ray og dr) = og + scale t dr


rayColor :: Ray -> Color
rayColor r
    | t > 0 =
        let n = normalize (rayAt t r - Vec3 0 0 (-1))
         in scale 0.5 (n & v3MapAll (+ 1))
    | otherwise =
        let normDir = normalize r.direction
            a = 0.5 * (normDir.y + 1)
         in scale (1 - a) (fromOne 0.9) + scale a (Vec3 0.5 0.75 1)
  where
    t = hitSphere (Vec3 0 0 (-1)) 0.5 r


hitSphere :: Vec3 -> Double -> Ray -> Double
hitSphere center radius ray
    | discriminant < 0 = -1
    | otherwise = (h - sqrt discriminant) / a
  where
    oc = center - ray.origin
    a = lenSquared ray.direction
    h = ray.direction · oc
    c = lenSquared oc - radius * radius
    discriminant = h * h - a * c
