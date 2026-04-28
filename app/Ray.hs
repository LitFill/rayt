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
    | otherwise = (-b - sqrt discriminant) / (2 * a)
  where
    oc = center - ray.origin
    a = ray.direction · ray.direction
    b = (-2) * ray.direction · oc
    c = oc · oc - radius * radius
    discriminant = b * b - 4 * a * c
