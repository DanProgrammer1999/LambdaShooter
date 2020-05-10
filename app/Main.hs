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
	putStrLn "Loading sprites..."

	idleAnimation  <- loadAnimation terroristIdlePath
	runAnimation   <- loadAnimation terroristRunPath
	deathAnimation <- loadAnimation terroristDeathPath
	jumpAnimation  <- loadAnimation terroristJumpPath
	bgPic <- loadPicture backgroundPath

	putStrLn "Sprites are correctly loaded."
	let jumpAnimationFive =
		 jumpAnimation {_frames = take 5 $ jumpAnimation ^. frames}
	let fallAnimation =
		 jumpAnimation { _frames = [(jumpAnimation ^. frames) !! 6]}
	let playerAnimationTable = [
		(Idle, idleAnimation),
		(Running, runAnimation),
		(Jumping, jumpAnimationFive),
		(Falling, fallAnimation),
		(Dying, deathAnimation)
		]
	
	let world = sampleWorld bgPic playerAnimationTable
	play FullScreen white simulationRate world renderWorld handleInput updateWorld 