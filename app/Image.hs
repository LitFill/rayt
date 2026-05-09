module Image where

import Lib


renderImage :: IO ()
renderImage = render imageWidth aspectRatio samplePerPxl maxDepth world
  where
    imageWidth = 400
    aspectRatio = 16 / 9
    samplePerPxl = 36
    maxDepth = 25
    world =
        [ Sphere (Vec3 0 0 (-1)) 0.5
        , Sphere (Vec3 0 (-100.5) (-1)) 100
        ]
