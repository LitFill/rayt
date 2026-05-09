{-# LANGUAGE PatternSynonyms #-}

module Interval
    ( Interval (..)
    , pattern (:..:)
    , contains
    , empty
    , infinity
    , size
    , surrounds
    , universe
    , clamp
    ) where

import Text.Printf (printf)


data Interval = Interval
    {intervalMin, intervalMax :: !Double}


infixr 5 :..:
pattern (:..:) :: Double -> Double -> Interval
pattern iMin :..: iMax = Interval iMin iMax
{-# COMPLETE (:..:) #-}


instance Show Interval where
    show (iMin :..: iMax) = printf "[%0.3f ... %0.3f]" iMin iMax


{-# INLINEABLE size #-}
size :: Interval -> Double
size (iMin :..: iMax) = iMax - iMin


{-# INLINEABLE contains #-}
contains :: Double -> Interval -> Bool
contains n (iMin :..: iMax) = n >= iMin && n <= iMax


{-# INLINEABLE surrounds #-}
surrounds :: Double -> Interval -> Bool
surrounds n (iMin :..: iMax) = n > iMin && n < iMax


{-# INLINEABLE clamp #-}
clamp :: Double -> Interval -> Double
clamp n (iMin :..: iMax)
    | n < iMin = iMin
    | n > iMax = iMax
    | otherwise = n


infinity :: Double
infinity = 1 / 0


empty :: Interval
empty = infinity :..: -infinity


universe :: Interval
universe = -infinity :..: infinity
