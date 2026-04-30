{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}

module Ray where

import Interval (Interval (..), surrounds)
import Vec3


data Ray = Ray
    {origin, direction :: !Vec3}
    deriving (Show)


rayAt :: Double -> Ray -> Vec3
rayAt t (Ray og dr) = og + scale t dr


data HitInfo = HitInfo
    { hitPoint :: Vec3
    , hitNormal :: Vec3
    , hitT :: Double
    , hitFrontFace :: Bool
    }
    deriving (Show)


mkHitInfo :: Ray -> Vec3 -> Vec3 -> Double -> HitInfo
mkHitInfo ray normOutwardNormal point t =
    let frontFace = ray.direction · normOutwardNormal < 0
        normal = if frontFace then normOutwardNormal else -normOutwardNormal
     in HitInfo
            { hitPoint = point
            , hitNormal = normal
            , hitT = t
            , hitFrontFace = frontFace
            }


class Hittable obj where
    hit :: Ray -> Interval -> obj -> Maybe HitInfo


data Sphere = Sphere
    { sphereCenter :: Vec3
    , sphereRadius :: Double
    }
    deriving (Show)


mkSphere :: Vec3 -> Double -> Sphere
mkSphere center radius =
    Sphere center (max 0 radius)


instance Hittable Sphere where
    hit ray rayT (Sphere center radius)
        | determinant < 0 = Nothing
        | not $ surrounds root rayT = Nothing
        | otherwise = Just info
      where
        oc = center - ray.origin
        a = lenSquared ray.direction
        h = ray.direction · oc
        c = lenSquared oc - radius * radius

        determinant = h * h - a * c

        sqrtd = sqrt determinant
        root' = (h - sqrtd) / a
        root
            | not $ surrounds root' rayT = (h + sqrtd) / a
            | otherwise = root'

        t = root
        p = rayAt t ray
        outwardNormal = (p - center) /^ radius
        info = mkHitInfo ray outwardNormal p t


-- instance (Hittable obj) => Hittable [obj] where
--     hit ray tmin tmax = go tmax Nothing
--       where
--         go _ acc [] = acc
--         go closest acc (obj : objs) =
--             let currentClosest = maybe closest hitT acc
--              in case hit ray tmin currentClosest obj of
--                     Just hitInfo -> go hitInfo.hitT (Just hitInfo) objs
--                     Nothing -> go closest acc objs

instance (Hittable obj) => Hittable [obj] where
    hit ray rayT = foldl' findClosest Nothing
      where
        findClosest acc obj =
            let currentTMax = maybe rayT.intervalMax hitT acc
             in case hit ray rayT {intervalMax = currentTMax} obj of
                    Just info -> Just info
                    Nothing -> acc
