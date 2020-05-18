{-# LANGUAGE TemplateHaskell #-}
module Animation where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Codec.Picture
import Codec.Picture.Extra
import Control.Lens
import System.Directory (getDirectoryContents)

import Constants
import Data.Bifunctor

data PlayerState = Idle | Running | Jumping | Falling | Dying deriving (Eq, Show) 
data Direction =  LeftDirection | RightDirection deriving (Eq, Show)

defaultFrameDelay :: Float
defaultFrameDelay = 0.15

data Animation = Animation
  { _frameDelay    :: Float      -- ^ How long to wait between frames
  , _frames        :: [Picture]  -- ^ All frames
  , _flippedFrames :: [Picture]  -- ^ Flipped frames (for Left Direction actions)
  , _waitFor       :: Float      -- ^ Time until next frame
  , _curFrame      :: Int        -- ^ Current number of frame
  }

makeLenses ''Animation

updateAnimation :: Float -> Animation -> Animation
updateAnimation elapsed a
  | elapsed > _waitFor a =
      a { _waitFor  = a ^. frameDelay
        , _curFrame = (a ^. curFrame + 1) `mod` length (a ^. frames)
        }
  | otherwise = a { _waitFor = (a ^. waitFor) - elapsed }

animationFromImages :: [Image PixelRGBA8] -> Animation
animationFromImages imgs = Animation {
    _frameDelay = defaultFrameDelay
  , _frames = pics
  , _flippedFrames = flippedPics
  , _waitFor = defaultFrameDelay -- maybe we should divide by simulationRate
  , _curFrame = 0
} where
  pics = map fromImageRGBA8 imgs
  flippedPics = map (fromImageRGBA8.flipHorizontally) imgs 

-- | loads all .pngs from a given folder and construct animation
loadAnimation :: FilePath -> IO Animation
loadAnimation filepath = do
  picFiles <- getDirectoryContents filepath
  let filteredPicFiles = filter (\e -> e `notElem` [".", ".."]) picFiles
  -- Better to use crossplatform concatenation and not '/' for unix-like only machines 
  let relativePicFiles = map ((filepath ++ "/") ++) filteredPicFiles
  images <- mapM loadImage relativePicFiles
  return $ animationFromImages images

-- TODO change to return IO (Either Image PixelRGBA8)
-- | loads single image from a given filepath.
loadImage :: FilePath -> IO (Image PixelRGBA8)
loadImage filepath = do 
  dynamicImage <- readPng filepath
  case dynamicImage of
      Right img -> return (convertRGBA8 img)
      Left err -> do
          
            let errorMsg = "Image \"" ++ filepath ++ "\" was not found."
            error (errorMsg ++ err)

-- | loads one picture from a given path.
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

dynamicImageToPicture :: DynamicImage -> Picture
dynamicImageToPicture = fromImageRGBA8.convertRGBA8

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

-- | makes default stab animation. Good to use with Maybe or Either
getDefaultAnimation :: Animation
getDefaultAnimation  = Animation {
    _frameDelay = defaultFrameDelay
  , _frames = [getDefaultPicture]
  , _flippedFrames = [getDefaultPicture]
  , _waitFor = defaultFrameDelay -- maybe we should divide by simulationRate
  , _curFrame = 0
}