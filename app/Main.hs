module Main where

import Logic
import Rendering
import Graphics.Gloss
import CommonData
import Textures
import Demo

simulationRate :: Int
simulationRate = 30

main :: IO ()
main = do
     textures <- map loadTexture texturesNames
     let table = zip texturesNames textures

     play FullScreen white simulationRate sampleWorld renderWorld handleInput updateWorld 