module Demo  where

import CommonData
import Graphics.Gloss hiding (Rectangle)

sampleWorld :: Picture -> Picture -> World
sampleWorld background playerTexture = World
 (Map background 200 300 []) [] (samplePlayer playerTexture)
     

samplePlayer :: Picture -> Entity
samplePlayer playerTexture =
     Entity playerBody playerTexture playerData  where
        playerData = PlayerData [] 0 100.0 0 "Alex" False
        playerBody = Body (0,0) (0,0) 10.0 (0, 0) (RectangleBox 10.0 20.0)
