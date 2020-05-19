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

-- | Receive events and update the world
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

-- | First, update my player according to buttons pressed 
-- | Second, update entities acceleration, velocity, and position; calculate gravity
-- | Third, check for collision
-- | Fourth, if there was collision, restore old position and clear acceleration and velocity
-- | Finally, if the collision was with projectile, subtract the value from the player's health
updateWorld :: Float -> World -> World
updateWorld = updateMyPlayer 


updateMyPlayer :: Float -> World -> World
updateMyPlayer timePassed world = world & myPlayer .~ newPlayer
    where
        oldPlayer = world ^. myPlayer

        (deltaVelocity, newDirection) = applyButtonsPress (world ^. keyboardData) oldPlayer
        deltaVelocity' = mulSV timePassed deltaVelocity

        oldVelocity = oldPlayer ^. entityBody . bodyVelocity
        newVelocity = oldVelocity &~ do
            _1 .= fst deltaVelocity'
            _2 += snd deltaVelocity'

        newPlayer = updateEntity timePassed (world ^. worldMap) withVelocity
            where
                withDirection = oldPlayer & direction .~ newDirection
                withVelocity = withDirection & entityBody . bodyVelocity .~ newVelocity


updateEntity :: Float -> Map -> Entity -> Entity
updateEntity timePassed map entity =
    entity &~ do
        entityBody %= updateBody timePassed map
        entityData . animations .= newAnimationTable
        entityData . currentState .= newState
    where
        oldState = fromMaybe Idle $ entity ^? entityData . currentState
        newState = getNewState entity
        newAnimationTable =
            if isPlayer entity
            then getNewAnimation timePassed entity (newState /= oldState)
            else []

getNewState :: Entity -> PlayerState
getNewState entity
    | snd velocity > stopVelocity    = Jumping
    | snd velocity < (-stopVelocity) = Falling
    | fst velocity > stopVelocity    = Running
    | fst velocity < (-stopVelocity) = Running
    | otherwise                      = Idle
    where
        velocity = entity ^. entityBody . bodyVelocity
        currState = entity ^? entityData . currentState

applyButtonsPress :: KeyboardInfo -> Entity -> (Velocity, Direction)
applyButtonsPress (KeyboardInfo rightKey leftKey jumpKey _) entity
    = ((xVelocity, yVelocity), newDirection)
    where
        velocityLens = entityBody . bodyVelocity
        (xVelocity, newDirection) =
            case (leftKey, rightKey) of
                (True, True)   -> (0, entity ^. direction)
                (False, False) -> (0, entity ^. direction)
                (True, False)  -> (entity ^. velocityLens . _1 - accelerationRate, LeftDirection)
                (False, True)  -> (entity ^. velocityLens . _1 + accelerationRate, RightDirection)
        yVelocity =
            if jumpKey
            then jumpAcceleration
            else 0

checkEntityCollision :: Entity -> Entity -> Bool
checkEntityCollision entity1 entity2 = detectCollision position1 position2 box1 box2
    where
        position1 = entity1 ^. entityBody . bodyPosition
        position2 = entity2 ^. entityBody . bodyPosition

        box1 = entity1 ^. entityBody . bodyCollisionBox
        box2 = entity2 ^. entityBody . bodyCollisionBox

getNewAnimation :: Float -> Entity -> Bool -> [(PlayerState, Animation)]
getNewAnimation timePassed player wasStateChange = newPlayerTable
    where
        -- | Update Animation should be done for all players
        curState = fromMaybe EmptyState (player ^? entityData . currentState)
        -- | Calculate new Animation
        oldPlayerAnimation = fromMaybe getDefaultAnimation $ getAnimationFromEntity player
        newPlayerAnimation = newAnimation where
            anim = updateAnimation timePassed oldPlayerAnimation
            newAnimation = if wasStateChange then anim {_curFrame = 0} else anim
        -- | Calculate new Animation Table
        oldPlayerTable = player ^. entityData . animations
        newPlayerTable = (curState, newPlayerAnimation)
            : filter (\(state, _) -> state /= curState) oldPlayerTable