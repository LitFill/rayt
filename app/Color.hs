{-# LANGUAGE PatternSynonyms #-}

module Color where

import Interval (clamp, pattern (:..:))
import Vec3 (Vec3 (Vec3))


type Color = Vec3


colorPrint :: Color -> String
colorPrint (Vec3 r g b) =
    let f = floor . (256 *) . flip clamp (0 :..: 0.999)
     in unwords $ map (show @Int . f) [r, g, b]
