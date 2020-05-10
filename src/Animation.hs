{-# LANGUAGE TemplateHaskell #-}
module Animation where

import Graphics.Gloss
import Control.Lens

defaultFrameDelay :: Float
defaultFrameDelay = 0.2

data Animation = Animation
  { _frameDelay  :: Float      -- ^ How long to wait between frames
  , _frames      :: [Picture]  -- ^ All frames
  , _waitFor     :: Float      -- ^ Time until next frame
  , _curFrame    :: Int        -- ^ Current number of frame
  }

makeLenses ''Animation

updateAnimation :: Float -> Animation -> Animation
updateAnimation elapsed a
  | elapsed > _waitFor a =
      a { _waitFor  = a ^. frameDelay
        , _curFrame = (a ^. curFrame + 1) `mod` length (a ^. frames)
        }
  | otherwise = a { _waitFor = (a ^. waitFor) - elapsed }

