{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}

module Vec3 where

import Data.Function ((&))


data Vec3 = Vec3
    {x, y, z :: !Double}
    deriving (Show)


instance Num Vec3 where
    (+) = add
    (-) = sub
    (*) = v3ZipWith (*)
    abs = normalize
    fromInteger i = let d = fromInteger i in Vec3 d d d
    signum (Vec3 x y z) = Vec3 (signum x) (signum y) (signum z)


add :: Vec3 -> Vec3 -> Vec3
add a b =
    Vec3 (a.x + b.x) (a.y + b.y) (a.z + b.z)


sub :: Vec3 -> Vec3 -> Vec3
sub a b =
    Vec3 (a.x - b.x) (a.y - b.y) (a.z - b.z)


v3MapAll :: (Double -> Double) -> Vec3 -> Vec3
v3MapAll f (Vec3 x y z) = Vec3 (f x) (f y) (f z)


v3ZipWith :: (Double -> Double -> Double) -> Vec3 -> Vec3 -> Vec3
v3ZipWith f a b = Vec3 (f a.x b.x) (f a.y b.y) (f a.z b.z)


v3Fold :: (Double -> Double -> Double) -> Vec3 -> Double
v3Fold f (Vec3 x y z) = x `f` y `f` z


add' :: Vec3 -> Vec3 -> Vec3
add' = v3ZipWith (+)


sub' :: Vec3 -> Vec3 -> Vec3
sub' = v3ZipWith (-)


scale :: Double -> Vec3 -> Vec3
scale t = v3MapAll (t *)


(/^) :: Vec3 -> Double -> Vec3
v /^ k = v3MapAll (/ k) v


len :: Vec3 -> Double
len (Vec3 x y z) = sqrt (x * x + y * y + z * z)


len' :: Vec3 -> Double
len' v = sqrt . v3Fold (+) $ v3ZipWith (*) v v


lenSquared :: Vec3 -> Double
lenSquared v = v & v3ZipWith (*) v & v3Fold (+)


dot :: Vec3 -> Vec3 -> Double
dot a b = a.x * b.x + a.y * b.y + a.z * b.z


(·) :: Vec3 -> Vec3 -> Double
(·) = dot


dot' :: Vec3 -> Vec3 -> Double
dot' a b = v3Fold (+) $ v3ZipWith (*) a b


cross :: Vec3 -> Vec3 -> Vec3
cross a b =
    Vec3
        (a.y * b.z - a.z * b.y)
        (a.z * b.x - a.x * b.z)
        (a.x * b.y - a.y * b.x)


(×) :: Vec3 -> Vec3 -> Vec3
(×) = cross


normalize :: Vec3 -> Vec3
normalize v = v3MapAll (/ len v) v


empty :: Vec3
empty = Vec3 0 0 0


fromOne :: Double -> Vec3
fromOne x = Vec3 x x x


data Ix = I0 | I1 | I2 deriving (Bounded, Enum, Eq, Ord, Show)


fromInt :: Int -> Ix
fromInt = \case
    0 -> I0
    1 -> I1
    2 -> I2
    n -> error $ "Ix out of bounds: " ++ show n


at :: Ix -> Vec3 -> Double
at = \case
    I0 -> x
    I1 -> y
    I2 -> z


(!) :: Vec3 -> Ix -> Double
v ! i = at i v


v3Print :: Vec3 -> String
v3Print (Vec3 x y z) =
    unwords $ map show [x, y, z]


aa, bb :: Vec3
aa = 1 --- Vec3 1 1 1
bb = Vec3 (-3) 2 2


main :: IO ()
main = do
    putStrLn "=== VECTOR3 ==="

    putStr "a     = "
    print aa
    putStr "a + a = "
    print $ aa `add` aa
    putStr "a - a = "
    print $ aa `sub` aa

    putStr "b     = "
    print bb
    putStr "a + b = "
    print $ aa `add` bb
    putStr "b - a = "
    print $ bb `sub` aa


type Color = Vec3


colorPrint :: Color -> String
colorPrint (Vec3 r g b) =
    let f = floor . (255.999 *)
     in unwords $ map (show @Int . f) [r, g, b]
