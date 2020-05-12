{-# LANGUAGE TemplateHaskell #-}
module Animation where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Control.Lens
import System.Directory (getDirectoryContents)
import Constants
import Data.Bifunctor
data PlayerState = Idle | Running | Jumping | Falling | Dying deriving (Eq, Show) 
data Direction =  LeftDirection | RightDirection deriving (Eq, Show)

defaultFrameDelay :: Float
defaultFrameDelay = 0.15

data Animation = Animation
  { _frameDelay  :: Float      -- ^ How long to wait between frames
  , _frames      :: [Picture]  -- ^ All frames
  , _waitFor     :: Float      -- ^ Time until next frame
  , _curFrame    :: Int        -- ^ Current number of frame
  } deriving(Show)

makeLenses ''Animation

updateAnimation :: Float -> Animation -> Animation
updateAnimation elapsed a
  | elapsed > _waitFor a =
      a { _waitFor  = a ^. frameDelay
        , _curFrame = (a ^. curFrame + 1) `mod` length (a ^. frames)
        }
  | otherwise = a { _waitFor = (a ^. waitFor) - elapsed }

animationFromPictures :: [Picture] -> Animation
animationFromPictures pics = Animation {
    _frameDelay = defaultFrameDelay
  , _frames = pics
  , _waitFor = defaultFrameDelay ---- ^ maybe we should divide by simulationRate
  ,_curFrame = 0
}

-- ^ loads all pictures from a given folder
loadAnimation :: FilePath -> IO Animation
loadAnimation filepath = do
  picFiles <- getDirectoryContents filepath
  let filteredPicFiles = filter (\e -> e `notElem` [".", ".."]) picFiles
  ---- ^ Better to use crossplatform symbol and not '/' for unix-only machines 
  let relativePicFiles = map ((filepath ++ "/") ++) filteredPicFiles
  frames <- mapM loadPicture relativePicFiles
  return $ animationFromPictures frames


-- ^ loads one picture from a given path.
loadPicture :: FilePath -> IO Picture 
loadPicture filepath = do 
  pic <- loadJuicyPNG filepath
  case pic of
      Just pic -> return pic
      Nothing -> do
          -- maybe we need to stop execution if smth goes wrong
            let errorMsg = "Picture \"" ++ filepath ++ "\" was not found."
            print errorMsg
            return blank

scaleAnimation :: Float  -> Animation -> Animation
scaleAnimation scaleFactor anima = anima {
    _frames = map (scale scaleFactor scaleFactor) (anima ^. frames)
}  

loadPlayerAnimations :: IO [(PlayerState, Animation)]
loadPlayerAnimations = do
    putStrLn "Loading player animations..."
    let stateAnimations =
         map (Data.Bifunctor.second loadAnimation) allTerroristAnimationPathes
    let states = map fst stateAnimations
    anims <- mapM snd stateAnimations
    let animationTable = zip states anims
    print states
    putStrLn "Player animations are loaded!"
    return animationTable

allTerroristAnimationPathes :: [(PlayerState, FilePath)]
allTerroristAnimationPathes = [
    (Idle, terroristIdlePath),
    (Running, terroristRunPath),
    (Dying, terroristDeathPath),
    (Jumping, terroristJumpPath),
    (Falling, terroristFallPath)
    ]

getDefaultAnimation :: Animation
getDefaultAnimation  = Animation {
    _frameDelay = defaultFrameDelay
  , _frames = [color yellow $ rectangleSolid 50 50]
  , _waitFor = defaultFrameDelay ---- ^ maybe we should divide by simulationRate
  ,_curFrame = 0
}

loadBlankAnimation :: IO Animation
loadBlankAnimation = loadAnimation blankAnimationPath