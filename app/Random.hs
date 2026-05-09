{-# LANGUAGE PatternSynonyms #-}

module Random
    ( random
    , randomI
    , randomR
    ) where

import System.Random (randomRIO)

import Interval (Interval, pattern (:..:))


-- | return a random Double in range [lo, hi)
randomR :: (Double, Double) -> IO Double
randomR =
    -- NOTE: randomRIO really returns [lo, hi] not [lo, hi) that randomR should be.
    -- so it could return 1.0 with the chance of 1e-53
    randomRIO


-- | return a random Double in range [0, 1)
random :: IO Double
random = randomR (0, 1)


randomI :: Interval -> IO Double
randomI (lo :..: hi) = randomR (lo, hi)
