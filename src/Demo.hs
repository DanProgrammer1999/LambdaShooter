module Demo  where

import Graphics.Gloss hiding (Rectangle)
import Prelude hiding (Left, Right)

import Animation 
import CommonData
import Constants

sampleWorld :: Picture -> [(PlayerState, Animation)] -> World
sampleWorld background animationTable = World
    (Map background 200 300 []) [] (makePlayer "Alex" animationTable) keyboardInfo
     

makePlayer :: String -> [(PlayerState, Animation)] -> Entity
makePlayer name animationTable =
     Entity playerBody Blank playerData RightDirection where
        playerData = PlayerData [] 0 100.0 0 name False Idle animationTable 
        playerBody = Body defaultPosition (0,0) 10.0 (0, 0) (RectangleBox 10.0 20.0)
