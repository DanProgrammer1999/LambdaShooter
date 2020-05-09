module Logic (handleInput, updateWorld) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Lens
import Data.Maybe

import CommonData
import Constants

data Direction
    = Left
    | Right

data Action
    = Move Direction
    | Jump

-- Receive events and update the world
handleInput :: Event -> World -> World
handleInput (EventKey (Char c) Down _ _) world = newWorld
    where
        positionLens = myPlayer . entityBody . bodyPosition

        newWorld =
            case c of
                'a' -> world & positionLens . _1 -~ movementSpeed
                'd' -> world & positionLens . _1 +~ movementSpeed
                ' ' -> world & myPlayer .~ makeJump (world ^. myPlayer)
                _   -> world
                
handleInput _ world = world

makeJump :: Entity -> Entity
makeJump player = player &~
    do
        entityData . hasJumped .= True 
        entityBody . bodyPosition . _2 += availableJumpHeight
    where 
        hasPlayerJumped = fromMaybe True (player ^? entityData . hasJumped)
        availableJumpHeight = 
            if not hasPlayerJumped 
            then jumpHeight 
            else 0
        


-- Update entities parameters (position, velocity, acceleration) based on time passed
-- Gravity calculations and collision detection is also here
updateWorld :: Float -> World -> World
updateWorld timePassed world = world
