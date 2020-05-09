module Demo  where

import CommonData
import Graphics.Gloss

sampleWorld :: Picture -> Picture -> World
sampleWorld background playerTexture = World
 (Map background 200 300 [])
 []
 (Entity playerTexture (Player playerData) (0,0) (0,0) (0,0)) where
     playerData = PlayerData [] 0 100 0 "Alex"
