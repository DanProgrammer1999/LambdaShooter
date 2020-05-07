module Main where

import Lib
import Graphics.Gloss

simulationRate :: Integer
simulationRate = 30

main :: IO ()
main = play FullScreen simulationRate 