module Demo  where

import Graphics.Gloss hiding (Rectangle)
import Prelude hiding (Left, Right)

import Animation 
import CommonData
import Constants
import Constructors (keyboardInfo, makePlayer)

sampleWorld :: ID -> Name -> World
sampleWorld uniqueID name
    = World worldMap' [] [] [] player keyboardInfo maxShootingCooldown
    where
        player = makePlayer uniqueID name
        worldMap' = Map sampleBlocks

sampleBlocks :: [Block]
sampleBlocks = 
    [
        Block  (0, -200)   800 100,
        Block  (200, 70)   200 30,
        Block  (-300, -60) 40  150,
        Block  (0, -50)     200 30
    ]
