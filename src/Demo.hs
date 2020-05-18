module Demo  where

import Graphics.Gloss hiding (Rectangle)
import Prelude hiding (Left, Right)

import Animation 
import CommonData
import Constants

sampleWorld :: Picture -> [(PlayerState, Animation)] -> World
sampleWorld background animationTable = World
    (Map background 1000000 10000 sampleMap) [] (makePlayer "Alex" animationTable) keyboardInfo

sampleMap = 
    [
        Block (0, -200) (sampleBlockTexture 800 100) 800 100 
    ]

sampleBlockTexture :: Float -> Float -> Picture
sampleBlockTexture w h = color red $ rectangleSolid (w*entitiesScale) (h*entitiesScale)

makePlayer :: String -> [(PlayerState, Animation)] -> Entity
makePlayer name animationTable =
     Entity playerBody Blank playerData RightDirection where
        playerData = PlayerData [] 0 100.0 0 name Idle animationTable 
        playerBody = Body defaultPosition (0,0) 10.0 (RectangleBox 10.0 40.0) False
