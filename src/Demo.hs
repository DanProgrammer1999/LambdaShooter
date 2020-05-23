module Demo  where

import Graphics.Gloss hiding (Rectangle)
import Prelude hiding (Left, Right)

import Animation 
import CommonData
import Constants
import Constructors (keyboardInfo, makePlayer)

sampleWorld :: Picture -> Picture  -> ID -> Name -> [(PlayerState, Animation)] -> World
sampleWorld background blockTexture uniqueID name animationTable 
    = World worldMap' [] [] player keyboardInfo maxShootingCooldown defaultWindowSize
    where
        player = makePlayer uniqueID name animationTable
        worldMap' = Map background (sampleMap blockTexture)

makeBlock :: Picture -> Position -> Float -> Float -> Block
makeBlock blockTexture position width height
    = Block position (scale xScale yScale blockTexture) width height
    where 
        (textureWidth, textureHeight) = blockTextureSize
        xScale = width / textureWidth
        yScale = height / textureHeight 

sampleMap :: Picture -> [Block]
sampleMap blockTexture = 
    [
        makeBlock blockTexture (0, -200)   800 100,
        makeBlock blockTexture (200, 70)   200 30,
        makeBlock blockTexture (-300, -60) 40  150,
        makeBlock blockTexture (0, -50)     200 30
    ]

sampleBlockTexture :: Float -> Float -> Picture
sampleBlockTexture w h = color blue $ rectangleSolid w h
