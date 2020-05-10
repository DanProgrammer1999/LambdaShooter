module Main where

import Data.Maybe
import Control.Lens
import Graphics.Gloss
import Graphics.Gloss.Juicy

import CommonData
import Demo (sampleWorld)
import Animation
import Constants
import System.Directory
import Logic
import Rendering


simulationRate :: Int
simulationRate = 30

main :: IO ()
main = do
    bgPic <- loadPicture backgroundPath
    playerAnimationTable <- loadPlayerAnimations 
    let world = sampleWorld bgPic playerAnimationTable
    putStrLn "Play!"
    play FullScreen white simulationRate world renderWorld handleInput updateWorld 