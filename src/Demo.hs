module Demo  where

import Graphics.Gloss hiding (Rectangle)
import Prelude hiding (Left, Right)

import Animation 
import CommonData

sampleWorld :: Picture -> [(PlayerState, Animation)] -> World
sampleWorld background animationTable = World
 (Map background 200 300 []) [] (makePlayer "Alex" animationTable)
     

makePlayer :: String -> [(PlayerState, Animation)] -> Entity
makePlayer name animationTable =
     Entity playerBody Blank playerData Right where
        playerData = PlayerData [] 0 100.0 0 name False Idle animationTable 
        playerBody = Body (0,0) (0,0) 10.0 (0, 0) (RectangleBox 10.0 20.0)
