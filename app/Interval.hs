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


size :: Interval -> Double
size (iMin :..: iMax) = iMax - iMin


contains :: Double -> Interval -> Bool
contains n (iMin :..: iMax) = n >= iMin && n <= iMax


surrounds :: Double -> Interval -> Bool
surrounds n (iMin :..: iMax) = n > iMin && n < iMax


infinity :: Double
infinity = 1 / 0


empty :: Interval
empty = infinity :..: -infinity


universe :: Interval
universe = -infinity :..: infinity
