module Main where

import System.Environment (getArgs)

import Image
import Lib


runVec :: IO ()
runVec = do test


runImage :: IO ()
runImage = do
    putStr renderImage


handleArgs :: [String] -> IO ()
handleArgs = \case
    ["vec"] -> runVec
    ["img"] -> runImage
    _ -> putStrLn "usage: rayt {vec,img}"


main :: IO ()
main = getArgs >>= handleArgs
