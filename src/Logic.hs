{-# LANGUAGE RecordWildCards #-}
module Logic (handleInput, updateWorld) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Control.Lens
import Data.Maybe
import Data.Either
import Data.Char

import CommonData
import Constants
import Animation
import Physics

-- ^ Receive events and update the world
handleInput :: Event -> World -> World
handleInput (EventKey (Char c) state _ _) world
    = world & keyboardData %~ keyAction (toLower c) (state == Down)
handleInput (EventKey (SpecialKey KeySpace) state _ _) world
    = world & keyboardData %~ keyAction ' ' (state == Down)
handleInput _ world = world

keyAction :: Char -> Bool -> KeyboardInfo -> KeyboardInfo
keyAction 'a' isDown = set leftKeyPressed isDown
keyAction 'd' isDown = set rightKeyPressed isDown
keyAction ' ' isDown = set jumpKeyPressed isDown
keyAction _ _        = id

-- ^ First, update my player according to buttons pressed 
-- ^ Second, update entities acceleration, velocity, and position; calculate gravity
-- ^ Third, check for collision
-- ^ Fourth, if there was collision, restore old position and clear acceleration and velocity
-- ^ Finally, if the collision was with projectile, subtract the value from the player's health
updateWorld :: Float -> World -> World
updateWorld timePassed world = world & myPlayer .~ newPlayer
    where
        oldPlayer = world ^. myPlayer

        newVelocity = mulSV timePassed $ applyButtonsPress (world ^. keyboardData)
    
        movedPlayer 
            = oldPlayer & entityBody . bodyVelocity . _1 .~ fst newVelocity
                        & entityBody . bodyVelocity . _2 +~ snd newVelocity
            
        newPlayer =  updateEntity timePassed (world ^. worldMap) movedPlayer

-- TODO TOFIX Daniel : LeftDirection is never observed.
updateEntity :: Float -> Map -> Entity -> Entity
updateEntity timePassed map entity = 
    entity &~ do
        entityBody %= updateBody timePassed map
        entityData . currentState .= newState
        direction .= newDirection 
        -- this will update only for players and ignored for other entities
        -- entityData . animations .= newAnimationTable
    where
        (newState, newDirection) = fromMaybe (Idle, RightDirection) (getState entity)
        newAnimationTable = 
            if isPlayer entity 
            then getAnimation timePassed entity
            else []

getState :: Entity -> Maybe (PlayerState, Direction)
getState entity
    | (entity ^. velocityLens . _2) > stopVelocity    = Just (Jumping, currDirection)
    | (entity ^. velocityLens . _2) < (-stopVelocity) = Just (Falling, currDirection)
    | (entity ^. velocityLens . _1) > stopVelocity    = Just (Running, RightDirection)
    | (entity ^. velocityLens . _1) < (-stopVelocity) = Just (Running, LeftDirection)
    | otherwise                                       = Just (Idle, currDirection)
    where 
        currDirection = entity ^. direction
        velocityLens = entityBody . bodyVelocity

applyButtonsPress :: KeyboardInfo -> Velocity
applyButtonsPress (KeyboardInfo rightKey leftKey jumpKey _) 
    = (0, 0) &~ 
    do
        _1 += case (leftKey, rightKey) of
                (True, True)   -> 0
                (False, False) -> 0
                (True, False)  -> -accelerationRate
                (False, True)  -> accelerationRate
        _2 += if jumpKey 
              then jumpAcceleration
              else 0

-- checkMapCollision :: Entity -> Map -> Bool
-- checkMapCollision entity (Map _ maxMapWidth maxMapHeight blocks) 
--     = _
--     where
--         blockPositions = 

checkEntityCollision :: Entity -> Entity -> Bool
checkEntityCollision entity1 entity2 = detectCollision position1 position2 box1 box2
    where
        position1 = entity1 ^. entityBody . bodyPosition
        position2 = entity2 ^. entityBody . bodyPosition

        box1 = entity1 ^. entityBody . bodyCollisionBox
        box2 = entity2 ^. entityBody . bodyCollisionBox

getAnimation :: Float -> Entity -> [(PlayerState, Animation)]
getAnimation timePassed player = newPlayerTable
    where
        -- ^ Update Animation should be done for all players
        curState = fromMaybe Idle (player ^? entityData . currentState)
        -- ^ Calculate new Animation
        oldPlayerAnimation = fromMaybe getDefaultAnimation $ getAnimationFromEntity player
        newPlayerAnimation = updateAnimation timePassed oldPlayerAnimation
        -- ^ Calculate new Animation Table
        oldPlayerTable = player ^. entityData . animations
        newPlayerTable = (curState, newPlayerAnimation)
            : filter (\(state, _) -> state == curState) oldPlayerTable