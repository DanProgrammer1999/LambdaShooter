module Main where

import Logic
import Rendering
import Graphics.Gloss
import CommonData
import Textures

simulationRate :: Int
simulationRate = 30

main :: IO ()
main = do
     textures <- map loadTexture texturesNames
     ()
    --  play FullScreen white simulationRate sampleWorld renderWorld handleInput updateWorld 