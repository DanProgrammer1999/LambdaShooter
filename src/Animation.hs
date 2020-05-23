{-# LANGUAGE OverloadedStrings #-}

module Animation where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Codec.Picture
import Codec.Picture.Extra
import Control.Lens 
import System.Directory (getDirectoryContents)
import Data.Aeson
import GHC.Generics

import Data.Bifunctor
import Data.List (sort)

import Constants
import Constructors (getDefaultPicture)
import CommonData

updateAnimation :: Float -> Animation -> Animation
updateAnimation elapsed a
  | elapsed > _waitFor a =
      a { _waitFor  = a ^. frameDelay
        , _curFrame = if _isOnce a
             then min (a ^. curFrame + 1) (length (a ^. frames) - 1)
             else (a ^. curFrame + 1) `mod` length (a ^. frames)
        }
  | otherwise = a { _waitFor = (a ^. waitFor) - elapsed }

animationFromImages :: [Image PixelRGBA8] -> Bool -> Animation
animationFromImages imgs isOnce = Animation {
    _frameDelay = defaultFrameDelay
  , _frames = pics
  , _flippedFrames = flippedPics
  , _waitFor = defaultFrameDelay -- maybe we should divide by simulationRate
  , _curFrame = 0
  , _isOnce = isOnce
} where
  pics = map fromImageRGBA8 imgs
  flippedPics = map (fromImageRGBA8.flipHorizontally) imgs 

-- | loads all .pngs from a given folder and construct animation
loadAnimation :: FilePath -> Bool -> IO Animation
loadAnimation filepath isOnce = do
  picFiles <- getDirectoryContents filepath
  let filteredPicFiles = sort $ filter (\e -> e `notElem` [".", ".."]) picFiles
  -- Better to use crossplatform concatenation and not '/' for unix-like only machines 
  let relativePicFiles = map ((filepath ++ "/") ++) filteredPicFiles
  images <- mapM loadImage relativePicFiles
  return $ animationFromImages images isOnce

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
scaleAnimation scaleFactor anima = anima & frames %~ map (scale scaleFactor scaleFactor)

loadPlayerAnimations :: IO PlayerAnimationTable
loadPlayerAnimations = do
    putStrLn "Loading player animations..."
    let stateAnimations = map f allPlayerAnimationsInfo
    let states = map fst stateAnimations;
    anims <- mapM snd stateAnimations
    let animationTable = zip states anims
    print states
    putStrLn "Player animations are loaded!"
    return animationTable where
        f (s, path, isOnce) =  (s, loadAnimation path isOnce)

allPlayerAnimationsInfo :: [(PlayerState, FilePath, Bool)]
allPlayerAnimationsInfo = [
    (Idle,     penguinIdlePath,     False),
    (Running,  penguinRunPath,      False),
    (Dying,    penguinDeathPath,    True),
    (Jumping,  penguinJumpPath,     True),
    (Falling,  penguinFallPath,     True),
    (Shooting, penguinShootingPath, True)
    ]

-- | makes default stab animation. Good to use with Maybe or Either
getDefaultAnimation :: Animation
getDefaultAnimation  = Animation 
    { _frameDelay = defaultFrameDelay
    , _frames = [getDefaultPicture]
    , _flippedFrames = [getDefaultPicture]
    , _waitFor = defaultFrameDelay
    , _curFrame = 0
    , _isOnce = False
    }