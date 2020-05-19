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
handleInput (EventKey (MouseButton LeftButton) state _ _) world 
    = world & keyboardData . fireKeyPressed .~ (state == Down)
handleInput _ world = world

keyAction :: Char -> Bool -> KeyboardInfo -> KeyboardInfo
keyAction 'a' isDown info = info & leftKeyPressed .~ isDown
keyAction 'd' isDown info = info & rightKeyPressed .~ isDown
keyAction ' ' isDown info = info & jumpKeyPressed .~ isDown
keyAction _ _        info = info

-- | First, update my player according to buttons pressed 
-- | Second, update entities acceleration, velocity, and position; calculate gravity
-- | Third, check for collision
-- | Fourth, if there was collision, restore old position and clear acceleration and velocity
-- | Finally, if the collision was with projectile, subtract the value from the player's health
updateWorld :: Float -> World -> World
updateWorld timePassed world = withNewPlayer & projectiles .~ newProjectiles
    where
        withNewPlayer = updateMyPlayer timePassed world

        oldProjectiles = world ^. projectiles
        newProjectiles = 
            if world ^. keyboardData . fireKeyPressed
            then newProjectile : oldProjectiles
            else oldProjectiles
            where newProjectile = shootBullet (withNewPlayer ^. myPlayer)

updateMyPlayer :: Float -> World -> World
updateMyPlayer timePassed world = world & myPlayer .~ newPlayer
    where
        oldPlayer = world ^. myPlayer
        keyboard = world ^. keyboardData

        deltaVelocity = getNewVelocity keyboard oldPlayer
        deltaVelocity' = mulSV timePassed deltaVelocity

        newDirection = getNewDirection keyboard (oldPlayer ^. direction)

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

shootBullet :: Entity -> Entity
shootBullet player = makeBullet defaultBulletPower bulletPosition (player ^. direction)
    where 
        playerPosition = player ^. entityBody . bodyPosition
        (RectangleBox w h) = player ^. entityBody . bodyCollisionBox 
        offsets = (w/2 + bulletOffset, h/2)
        directionMultiplier = 
            case player ^. direction of 
                LeftDirection -> -1
                RightDirection -> 1

        bulletPosition = addPoints playerPosition (mulSV directionMultiplier offsets)        

getNewState :: Entity -> PlayerState
getNewState entity
    | snd velocity > stopVelocity      = Jumping
    | snd velocity < (-stopVelocity)   = Falling
    | currState == Just Jumping        = Jumping
    | abs(fst velocity) > stopVelocity = Running
    | otherwise                        = Idle
    where
        velocity = entity ^. entityBody . bodyVelocity
        currState = entity ^? entityData . currentState

getNewDirection :: KeyboardInfo -> Direction -> Direction
getNewDirection (KeyboardInfo rightKey leftKey _ _) currDirection
    = case (leftKey, rightKey) of
        (True, False) -> LeftDirection
        (False, True) -> RightDirection
        _             -> currDirection

-- | Should only be used on my player 
getNewVelocity :: KeyboardInfo -> Entity -> Velocity
getNewVelocity (KeyboardInfo rightKey leftKey jumpKey _) entity
    = (xVelocity, yVelocity)
    where
        -- velocityLens = entityBody . bodyVelocity
        xVelocity =
            case (leftKey, rightKey) of
                (True, False)  -> -accelerationRate
                (False, True)  -> accelerationRate
                _              -> 0

        currState = entity ^? entityData . currentState
        jumpAllowed = currState == Just Idle || currState == Just Running
        -- jumpAllowed = True
        yVelocity =
            if jumpKey && jumpAllowed
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