{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}

module Ray where

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
         in scale 0.5 (n + 1)
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
    hit :: Ray -> Double -> Double -> obj -> Maybe HitInfo


data Sphere = Sphere
    { sphereCenter :: Vec3
    , sphereRadius :: Double
    }
    deriving (Show)


mkSphere :: Vec3 -> Double -> Sphere
mkSphere center radius =
    Sphere center (max 0 radius)


instance Hittable Sphere where
    hit ray tmin tmax (Sphere center radius)
        | determinant < 0 = Nothing
        | isNotInValidInterval root = Nothing
        | otherwise = Just info
      where
        isNotInValidInterval x = x <= tmin || x >= tmax

        oc = center - ray.origin
        a = lenSquared ray.direction
        h = ray.direction · oc
        c = lenSquared oc - radius * radius

        determinant = h * h - a * c

        sqrtd = sqrt determinant
        root' = (h - sqrtd) / a
        root
            | isNotInValidInterval root' = (h + sqrtd) / a
            | otherwise = root'

        t = root
        p = rayAt t ray
        outwardNormal = (p - center) /^ radius
        info = mkHitInfo ray outwardNormal p t
