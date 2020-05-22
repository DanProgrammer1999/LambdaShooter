module Main where

import Data.Maybe
import Control.Lens
import System.Environment
import System.Exit
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy


import CommonData
import Demo (sampleWorld)
import Animation
import Constants
import System.Directory
import Logic
import Rendering
import Server (serverMain)
import Client (clientMain)


simulationRate :: Int
simulationRate = 30

main :: IO ()
main = release

release :: IO ()
release = do
    bgPic <- loadPicture backgroundPath
    playerAnimationTable <- loadPlayerAnimations
    let world = sampleWorld bgPic playerAnimationTable (RectangleBox 40 80)
    play FullScreen white simulationRate world renderWorld handleInput updateWorld

debug :: IO ()
debug = do
    playerAnimationTable <- loadPlayerAnimations
    -- blank background (we know it loads correctly)
    let world = sampleWorld blank playerAnimationTable (RectangleBox 40 80)

    putStrLn "Starting..."
    playIO (InWindow "LambdaShooter" (1280, 720) (0,0)) white simulationRate
     world renderWorldIO handleInputIO updateWorldIO

renderWorldIO :: World -> IO Picture
renderWorldIO = return . renderWorld

handleInputIO :: Event -> World -> IO World
handleInputIO event world
    =  putStrLn ("Event happened: " ++ show event)
    >> return (handleInput event world)

updateWorldIO :: Float -> World -> IO World
updateWorldIO timePassed world 
    =  putStrLn ("Time since last frame: " ++ show timePassed)
    >> putStrLn ("My player details " ++ (world ^. myPlayer & show))
    >> putStrLn ("Projectiles: " ++ show (world ^. projectiles))
    >> return (updateWorld timePassed world) 





