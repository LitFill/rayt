{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Vec3 where

import Data.Function ((&))

import Interval
import Random (random, randomI)


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


{-# INLINEABLE add #-}
add :: Vec3 -> Vec3 -> Vec3
add a b =
    Vec3 (a.x + b.x) (a.y + b.y) (a.z + b.z)


{-# INLINEABLE sub #-}
sub :: Vec3 -> Vec3 -> Vec3
sub a b =
    Vec3 (a.x - b.x) (a.y - b.y) (a.z - b.z)


{-# INLINEABLE v3MapAll #-}
v3MapAll :: (Double -> Double) -> Vec3 -> Vec3
v3MapAll f (Vec3 x y z) = Vec3 (f x) (f y) (f z)


{-# INLINEABLE v3ZipWith #-}
v3ZipWith :: (Double -> Double -> Double) -> Vec3 -> Vec3 -> Vec3
v3ZipWith f a b = Vec3 (f a.x b.x) (f a.y b.y) (f a.z b.z)


{-# INLINEABLE v3Fold #-}
v3Fold :: (Double -> Double -> Double) -> Vec3 -> Double
v3Fold f (Vec3 x y z) = x `f` y `f` z


add' :: Vec3 -> Vec3 -> Vec3
add' = v3ZipWith (+)


sub' :: Vec3 -> Vec3 -> Vec3
sub' = v3ZipWith (-)


{-# INLINEABLE scale #-}
scale :: Double -> Vec3 -> Vec3
scale t = v3MapAll (t *)


{-# INLINEABLE (/^) #-}
(/^) :: Vec3 -> Double -> Vec3
v /^ k = v3MapAll (/ k) v


{-# INLINEABLE len #-}
len :: Vec3 -> Double
len (Vec3 x y z) = sqrt (x * x + y * y + z * z)


len' :: Vec3 -> Double
len' v = sqrt . v3Fold (+) $ v3ZipWith (*) v v


{-# INLINEABLE lenSquared #-}
lenSquared :: Vec3 -> Double
lenSquared v = v & v3ZipWith (*) v & v3Fold (+)


{-# INLINEABLE dot #-}
dot :: Vec3 -> Vec3 -> Double
dot a b = a.x * b.x + a.y * b.y + a.z * b.z


{-# INLINEABLE (·) #-}
(·) :: Vec3 -> Vec3 -> Double
(·) = dot


dot' :: Vec3 -> Vec3 -> Double
dot' a b = v3Fold (+) $ v3ZipWith (*) a b


{-# INLINEABLE cross #-}
cross :: Vec3 -> Vec3 -> Vec3
cross a b =
    Vec3
        (a.y * b.z - a.z * b.y)
        (a.z * b.x - a.x * b.z)
        (a.x * b.y - a.y * b.x)


{-# INLINEABLE (×) #-}
(×) :: Vec3 -> Vec3 -> Vec3
(×) = cross


{-# INLINEABLE normalize #-}
normalize :: Vec3 -> Vec3
normalize v = v3MapAll (/ len v) v


{-# INLINEABLE unitize #-}
unitize :: Vec3 -> Vec3
unitize = normalize


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


v3Random :: IO Vec3
v3Random = do
    x <- random
    y <- random
    z <- random
    pure Vec3 {..}


v3RandomI :: Interval -> IO Vec3
v3RandomI iv = do
    x <- randomI iv
    y <- randomI iv
    z <- randomI iv
    pure Vec3 {..}


v3RandomUnit :: IO Vec3
v3RandomUnit = do
    v <- v3RandomI (-1 :..: 1)
    let lenSq = lenSquared v
    if 1e-160 < lenSq && lenSq <= 1
        then pure $ v /^ sqrt lenSq
        else v3RandomUnit


v3RandomOnHemisphere :: Vec3 -> IO Vec3
v3RandomOnHemisphere normal = do
    onUnitSphere <- v3RandomUnit
    if onUnitSphere `dot` normal > 0
        then return onUnitSphere
        else return (-onUnitSphere)


aa, bb :: Vec3
aa = 1 --- Vec3 1 1 1
bb = Vec3 (-3) 2 2


test :: IO ()
test = do
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
