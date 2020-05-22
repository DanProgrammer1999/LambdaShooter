module Demo  where

import Graphics.Gloss hiding (Rectangle)
import Prelude hiding (Left, Right)

import Animation 
import CommonData
import Constants

sampleWorld :: Picture  -> ID -> Name -> [(PlayerState, Animation)] -> CollisionBox-> World
sampleWorld background uniqueID name animationTable  playerColBox = World
    (Map background 1000000 10000 sampleMap) []
     (makePlayer uniqueID name animationTable playerColBox) keyboardInfo

sampleMap = 
    [
        Block (0, -200) (sampleBlockTexture 800 100) 800 100,
        Block (200, 30) (sampleBlockTexture 200 40) 200 40 
    ]

sampleBlockTexture :: Float -> Float -> Picture
sampleBlockTexture w h = color red $ rectangleSolid w h


makePlayer :: ID -> Name -> [(PlayerState, Animation)] -> CollisionBox -> Entity
makePlayer uniqueID name animationTable  colBox =
     Entity uniqueID playerBody Blank playerData RightDirection where
        playerData = PlayerData [] 0 100.0 0 name Idle animationTable 
        playerBody = Body defaultPosition defaultVelocity 10.0 colBox False
