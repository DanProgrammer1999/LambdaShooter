module Demo  where

import Graphics.Gloss hiding (Rectangle)
import Prelude hiding (Left, Right)

import Animation 
import CommonData
import Constants
import Constructors (keyboardInfo, makePlayer)

sampleWorld :: Picture  -> ID -> Name -> [(PlayerState, Animation)] -> CollisionBox-> World
sampleWorld background uniqueID name animationTable playerColBox 
    = World worldMap' [] [] player keyboardInfo maxShootingCooldown defaultWindowSize
    where
        player = makePlayer uniqueID name animationTable
        worldMap' = Map background sampleMap

makeBlock :: Position -> Float -> Float -> Block
makeBlock position width height 
    = Block position (sampleBlockTexture width height) width height

sampleMap = 
    [
        makeBlock (0, -200)   800 100,
        makeBlock (200, 60)   200 30,
        makeBlock (-300, -60) 40  150,
        makeBlock (0, -60)     200 30
    ]

sampleBlockTexture :: Float -> Float -> Picture
sampleBlockTexture w h = color blue $ rectangleSolid w h
