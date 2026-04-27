module Image where

import Data.Function ((&))
import Text.Printf (printf)

import Ray
import Vec3


renderImage :: String
renderImage =
    let
        aspectRatio = 16 / 9 :: Double
        imgWidth = 400 * 2 :: Int
        height' = floor (fromIntegral imgWidth / aspectRatio)
        imgHeight = max height' 1 :: Int

        focalLength = 1
        -- viewports
        vpHeight = 2
        vpWidth = vpHeight * (fromIntegral imgWidth / fromIntegral imgHeight)

        cameraCenter = fromOne 0

        vpU = Vec3 vpWidth 0 0
        vpV = Vec3 0 (-vpHeight) 0

        pxlDeltaU = vpU & v3MapAll (/ fromIntegral imgWidth)
        pxlDeltaV = vpV & v3MapAll (/ fromIntegral imgHeight)

        vpUpperLeft =
            cameraCenter
                `sub` Vec3 0 0 focalLength
                `sub` (vpU & v3MapAll (/ 2))
                `sub` (vpV & v3MapAll (/ 2))

        pxl00Loc = vpUpperLeft + fromOne 0.5 * (pxlDeltaU + pxlDeltaV)

        header = printf "P3\n%d %d\n255\n" imgWidth imgHeight

        w = fromIntegral (imgWidth - 1)
        h = fromIntegral (imgHeight - 1)

        pixels =
            [ colorPrint pxlColor
            | y <- [0 .. h]
            , x <- [0 .. w]
            , let pxlCenter = pxl00Loc + fromOne x * pxlDeltaU + fromOne y * pxlDeltaV
            , let rayDirection = pxlCenter - cameraCenter
            , let ray = Ray cameraCenter rayDirection
            , let pxlColor = rayColor ray
            ]
     in
        header ++ unlines pixels
