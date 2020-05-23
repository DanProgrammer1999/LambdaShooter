module Demo  where

import Graphics.Gloss hiding (Rectangle)
import Prelude hiding (Left, Right)

import Animation 
import CommonData
import Constants
import Constructors (keyboardInfo, makePlayer)

sampleWorld :: Picture  -> ID -> Name -> [(PlayerState, Animation)] -> CollisionBox-> World
sampleWorld background uniqueID name animationTable playerColBox 
    = World worldMap' [] [] player keyboardInfo maxShootingCooldown
    where
        player = makePlayer uniqueID name animationTable
        worldMap' = Map background sampleMap

sampleMap = 
    [
        Block (0, -200)    800 100,
        Block (200,  30)   200 40,
        Block (-400, -50)  60  200
    ]

sampleBlockTexture :: Float -> Float -> Picture
sampleBlockTexture w h = color blue $ rectangleSolid w h
