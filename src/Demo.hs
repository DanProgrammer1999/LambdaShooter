module Demo  where

import Graphics.Gloss hiding (Rectangle)
import Prelude hiding (Left, Right)

import Animation 
import CommonData
import Constants

sampleWorld :: Picture -> [(PlayerState, Animation)] -> CollisionBox-> World
sampleWorld background animationTable playerColBox = World
    (Map background 1000000 10000 sampleMap) [] []
    (makePlayer "Alex" animationTable playerColBox) keyboardInfo 0

sampleMap = 
    [
        Block (0, -200) (sampleBlockTexture 800 100) 800 100,
        Block (200, 30) (sampleBlockTexture 200 40) 200 40 
    ]

sampleBlockTexture :: Float -> Float -> Picture
sampleBlockTexture w h = color red $ rectangleSolid w h


-- TODO TOFIX (ALL) "Make a correct CollisionBox for players".
makePlayer :: String -> [(PlayerState, Animation)] -> CollisionBox -> Entity
makePlayer name animationTable  colBox =
     Entity playerBody Blank playerData RightDirection where
        playerData = PlayerData [] 0 100.0 0 name Idle animationTable 
        playerBody = Body defaultPosition defaultVelocity playerWeight colBox False
