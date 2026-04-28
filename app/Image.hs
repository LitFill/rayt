{-# LANGUAGE OverloadedRecordDot #-}

module Image where

import Text.Printf (printf)

import Ray
import Vec3


data Camera = Camera
    { imgWidth :: Int
    , imgHeight :: Int
    , center :: Vec3
    , pixel00 :: Vec3
    , deltaU :: Vec3
    , deltaV :: Vec3
    }
    deriving (Show)


initCamera :: Int -> Double -> Camera
initCamera width aspect =
    let height = max 1 (floor $ fromIntegral width / aspect)
        -- viewport
        vpH = 2.0
        vpW = vpH * (fromIntegral width / fromIntegral height)
        focalLength = 1
        camCenter = 0

        -- viewport vecor U → and V ↓
        vu = Vec3 vpW 0 0
        vv = Vec3 0 (-vpH) 0

        -- pixel deltas
        du = vu /^ fromIntegral width
        dv = vv /^ fromIntegral height

        vUpLeft =
            camCenter -- the center  (0,0)
                - vu /^ 2 -- the X element
                - vv /^ 2 -- the Y element
                - Vec3 0 0 focalLength -- the Z element
     in Camera
            { imgWidth = width
            , imgHeight = height
            , center = camCenter
            , pixel00 = vUpLeft + fromOne 0.5 * (du + dv)
            , deltaU = du
            , deltaV = dv
            }


getRay :: Camera -> Int -> Int -> Ray
getRay cam x y =
    let pxlCenter =
            cam.pixel00
                + fromIntegral x * cam.deltaU
                + fromIntegral y * cam.deltaV
        rayDir = pxlCenter - cam.center
     in Ray cam.center rayDir


renderImage :: String
renderImage =
    let cam = initCamera 800 (16 / 9)
        header = printf "P3\n%d %d\n255\n" cam.imgWidth cam.imgHeight

        pixels =
            [ colorPrint $ rayColor (getRay cam x y)
            | y <- [0 .. cam.imgHeight - 1]
            , x <- [0 .. cam.imgWidth - 1]
            ]
     in header ++ unlines pixels
