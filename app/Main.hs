module Main where

import Data.Maybe
import Logic
import Rendering
import Graphics.Gloss
import Graphics.Gloss.Juicy
import CommonData
import Textures
import Demo (sampleWorld)

simulationRate :: Int
simulationRate = 30

main :: IO ()
main = do
     putStrLn "Loading textures..."
     textures <- mapM loadTexture textureNames
     let table = zip textureNames textures
     let bgTxt = fromJust $ backgroundTexture table
     let playerTxt = fromJust $ playerTexture table
     putStrLn "Textures are correctly loaded."
     let world = sampleWorld bgTxt playerTxt

     play FullScreen white simulationRate world renderWorld handleInput updateWorld 