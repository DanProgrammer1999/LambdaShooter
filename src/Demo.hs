module Demo  where

import Graphics.Gloss hiding (Rectangle)
import Prelude hiding (Left, Right)

import Animation (Animation)
import CommonData

sampleWorld :: Picture -> [(PlayerState, Animation)] -> World
sampleWorld background animationTable = World
 (Map background 200 300 []) [] (samplePlayer animationTable)
     

samplePlayer :: [(PlayerState, Animation)] -> Entity
samplePlayer animationTable =
     Entity playerBody Blank playerData  where
        playerData = PlayerData [] 0 100.0 0 "Alex" False Idle Right animationTable 
        playerBody = Body (0,0) (0,0) 10.0 (0, 0) (RectangleBox 10.0 20.0)
