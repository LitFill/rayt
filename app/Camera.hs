{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Camera (Camera, render) where

import Text.Printf (printf)

import Interval (infinity, pattern (:..:))
import Ray (HitInfo (hitNormal), Hittable (..), Ray (..))
import Vec3 (Vec3 (..), colorPrint, normalize, scale, (/^))


-- | Camera
data Camera = Camera
    { imgH :: Int
    -- ^ Rendered image height
    , center :: Vec3
    -- ^ Camera center
    , pixel00 :: Vec3
    -- ^ Location of pixel (0, 0)
    , dU :: Vec3
    -- ^ Offset to pixel to the right
    , dV :: Vec3
    -- ^ Offset to pixel below
    }


render :: (Hittable obj) => Int -> Double -> obj -> String
render imgW aspect world = header ++ unlines pixels
  where
    cam = initialize aspect imgW
    header = printf "P3\n%d %d\n255\n" imgW cam.imgH

    pixels =
        [ colorPrint . rayColor world $ getRay cam x y
        | y <- [0 .. cam.imgH - 1]
        , x <- [0 .. imgW - 1]
        ]


getRay :: Camera -> Int -> Int -> Ray
getRay cam x y = Ray {..}
  where
    pxlCenter =
        cam.pixel00
            + fromIntegral x * cam.dU
            + fromIntegral y * cam.dV
    origin = cam.center
    direction = pxlCenter - cam.center


initialize :: Double -> Int -> Camera
initialize aspect width = Camera {..}
  where
    imgH = max 1 (floor $ fromIntegral width / aspect)
    center = 0

    -- Viewport dimensions
    focalLength = 1
    vpH = 2.0
    vpW = vpH * (fromIntegral width / fromIntegral imgH)

    -- viewport vecor U → and V ↓
    vu = Vec3 vpW 0 0
    vv = Vec3 0 (-vpH) 0

    -- pixel deltas
    dU = vu /^ fromIntegral width
    dV = vv /^ fromIntegral imgH

    vUpLeft =
        center
            - vu /^ 2
            - vv /^ 2
            - Vec3 0 0 focalLength
    pixel00 = vUpLeft + scale 0.5 (dU + dV)


rayColor :: (Hittable obj) => obj -> Ray -> Vec3
rayColor world ray =
    -- maybe lerped (\i -> scale 0.5 (i.hitNormal + 1)) info
    maybe lerped (scale 0.5 . (+ 1) . hitNormal) info
  where
    info = hit ray (0 :..: infinity) world
    normDirection = normalize ray.direction
    a = 0.5 * (normDirection.y + 1)
    lerped = scale (1 - a) 1 + scale a (Vec3 0.5 0.75 1)
