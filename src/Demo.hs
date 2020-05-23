module Demo  where

import Graphics.Gloss hiding (Rectangle)
import Prelude hiding (Left, Right)

import Animation 
import CommonData
import Constants
import Constructors (keyboardInfo, makePlayer)

sampleWorld :: ID -> Name -> CollisionBox -> World
sampleWorld uniqueID name playerColBox 
    = World worldMap' [] [] [] player keyboardInfo maxShootingCooldown
    where
        player = makePlayer uniqueID name
        worldMap' = Map 2000 2000 sampleBlocks

sampleBlocks :: [Block]
sampleBlocks = 
    [
        Block (0, -200)    800 100,
        Block (200,  30)   200 40,
        Block (-400, -50)  60  200
    ]

sampleBlockTexture :: Float -> Float -> Picture
sampleBlockTexture w h = color blue $ rectangleSolid w h
