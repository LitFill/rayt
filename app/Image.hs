module Image where

import Lib


renderImage :: String
renderImage = render imageWidth aspectRatio world
  where
    imageWidth = 400 * 2
    aspectRatio = (16 / 9)
    world =
        [ Sphere (Vec3 0 0 (-1)) 0.5
        , Sphere (Vec3 0 (-100.5) (-1)) 100
        ]
