module Main where

import Logic
import Rendering
import Graphics.Gloss

simulationRate :: Integer
simulationRate = 30

main :: IO ()
main = play FullScreen simulationRate sampleWorld renderWorld handleInput updateWorld 