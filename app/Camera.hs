{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Camera (Camera, render) where

import Control.Monad (forM_, replicateM)
import Text.Printf (printf)

import Color (colorPrint)
import Interval (infinity, pattern (:..:))
import Random (random)
import Ray (HitInfo (..), Hittable (..), Ray (..))
import Vec3
    ( Vec3 (..)
    , scale
    , unitize
    , v3MapAll
    , v3RandomOnHemisphere
    , (/^)
    )


-- | Camera
data Camera = Camera
    { imgH :: Int
    -- ^ Rendered image height
    , pixelSamplesScale :: Double
    -- ^ Color scale factor for a sum of pixel samples
    , center :: Vec3
    -- ^ Camera center
    , pixel00 :: Vec3
    -- ^ Location of pixel (0, 0)
    , dU :: Vec3
    -- ^ Offset to pixel to the right
    , dV :: Vec3
    -- ^ Offset to pixel below
    }


render :: (Hittable obj) => Int -> Double -> Int -> Int -> obj -> IO ()
render imgW aspect samplePerPxl maxDepth world = do
    printf "P3\n%d %d\n255\n" imgW cam.imgH

    forM_ [0 .. cam.imgH - 1] \y ->
        forM_ [0 .. imgW - 1] \x ->
            renderPixel x y
  where
    cam = initialize aspect imgW samplePerPxl
    renderPixel x y = do
        let sampleLoop 0 !acc = pure acc
            sampleLoop n !acc = do
                ray <- getRay cam x y
                c <- rayColor maxDepth world ray
                sampleLoop (n - 1) (acc + c)

        sumColor <- sampleLoop samplePerPxl 0

        let pixelColor = scale cam.pixelSamplesScale sumColor
            gammaCorrected = v3MapAll (max 0) pixelColor

        putStrLn $ colorPrint gammaCorrected


render' :: (Hittable obj) => Int -> Double -> Int -> Int -> obj -> IO String
render' imgW aspect samplePerPxl maxDepth world = do
    let
        cam = initialize aspect imgW samplePerPxl
        header = printf "P3\n%d %d\n255\n" imgW cam.imgH

        pixels =
            [ (x, y)
            | y <- [0 .. cam.imgH - 1]
            , x <- [0 .. imgW - 1]
            ]

    let renderPixel (x, y) = do
            colors <- replicateM samplePerPxl $ do
                ray <- getRay cam x y
                rayColor maxDepth world ray
            let pixelColor = scale cam.pixelSamplesScale (sum colors)
                -- return $ maybe lerped xx info
                -- Apply the linear to gamma transform (gamma 2.2 for sRGB)
                gammaCorrected = v3MapAll (max 0) pixelColor
            return $ colorPrint gammaCorrected

    pxls <- traverse renderPixel pixels
    return $ header ++ unlines pxls


{- | Construct a camera ray originating originating from the origin and
directed at randomly sampled point around the pixel location (x, y)
-}
{-# INLINABLE getRay #-}
getRay :: Camera -> Int -> Int -> IO Ray
getRay cam x y = do
    offset <- sampleSquare
    let pxlSample =
            cam.pixel00
                + scale (fromIntegral x + offset.x) cam.dU
                + scale (fromIntegral y + offset.y) cam.dV
        origin = cam.center
        direction = pxlSample - origin
    return Ray {..}


-- getRay' :: Camera -> Int -> Int -> Ray
-- getRay' cam x y = Ray {..}
--   where
--     pxlCenter =
--         cam.pixel00
--             + fromIntegral x * cam.dU
--             + fromIntegral y * cam.dV
--     origin = cam.center
--     direction = pxlCenter - cam.center

initialize :: Double -> Int -> Int -> Camera
initialize aspect width samplePerPxl = Camera {..}
  where
    imgH = max 1 (floor $ fromIntegral width / aspect)
    pixelSamplesScale = 1 / fromIntegral samplePerPxl
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


rayColor :: (Hittable obj) => Int -> obj -> Ray -> IO Vec3
rayColor depth world ray
    | depth <= 0 = return 0
    | otherwise = do
        case info of
            Just i -> do
                direction <- v3RandomOnHemisphere i.hitNormal
                color <- rayColor (depth - 1) world (Ray i.hitPoint direction)
                return $ scale 0.5 color
            Nothing -> return lerped
  where
    info = hit ray (0 :..: infinity) world
    unitDirection = unitize ray.direction
    a = 0.5 * (unitDirection.y + 1)
    lerped = scale (1 - a) 1 + scale a (Vec3 0.5 0.75 1)


{- | Returns the vector to the random point in the inclusive range
(-.5, +.5) - (-.5, +.5)
-}
{-# INLINABLE sampleSquare #-}
sampleSquare :: IO Vec3
sampleSquare = do
    r1 <- random
    r2 <- random
    pure $ Vec3 (r1 - 0.5) (r2 - 0.5) 0
