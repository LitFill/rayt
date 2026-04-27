{-# LANGUAGE StrictData #-}

module Main (Main.main) where

import System.Environment (getArgs)

import Image
import Vec3


runVec :: IO ()
runVec = do
    Vec3.main


runImage :: IO ()
runImage = do
    putStr renderImage


handleArgs :: [String] -> IO ()
handleArgs = \case
    ["vec"] -> runVec
    ["img"] -> runImage
    _ -> putStrLn "usage: <program> {vec,img}"


main :: IO ()
main = getArgs >>= handleArgs
