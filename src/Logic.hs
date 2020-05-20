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

updateWorld :: Float -> World -> World
updateWorld timePassed world = withUpdatedProjectiles
    where
        withNewPlayer = updateMyPlayer timePassed world
        withUpdatedProjectiles = withNewPlayer & projectiles %~ fmap updateProjectile

        updateProjectile :: Entity -> Entity
        updateProjectile = over entityBody $ updateBody timePassed (world ^. worldMap)

updateMyPlayer :: Float -> World -> World
updateMyPlayer timePassed world = world &~
    do
        myPlayer .= newPlayer
        myPlayer . entityData . animations .= newAnimations
        myPlayer . entityData . currentState .= newState
        myPlayer . direction %= getNewDirection keyboard

        projectiles %=
            (\oldProjectiles ->
                if willShoot then createBullet newPlayer : oldProjectiles else oldProjectiles
            )
        shootingCooldown %=
            (\cooldown -> if willShoot then maxShootingCooldown else max 0 (cooldown - timePassed))
    where
        keyboard = world ^. keyboardData
        oldPlayer = world ^. myPlayer

        oldVelocity = oldPlayer ^. entityBody . bodyVelocity
        deltaVelocity = mulSV timePassed $ getNewVelocity keyboard oldPlayer
        newVelocity = deltaVelocity & _2 +~ snd oldVelocity

        newPlayer = oldPlayer &~ do
            entityBody . bodyVelocity .= newVelocity
            entityBody %= updateBody timePassed (world ^. worldMap)

        oldState = fromMaybe Idle $ oldPlayer ^? entityData . currentState
        newState = getNewState newPlayer

        newAnimations = getNewAnimation timePassed newPlayer (newState /= oldState)

        willShoot = canShoot && requestedShoot
            where
                requestedShoot = world ^. keyboardData . fireKeyPressed
                canShoot = world ^. shootingCooldown == 0

createBullet :: Entity -> Entity
createBullet player = makeBullet defaultBulletPower bulletPosition (player ^. direction)
    where
        (x, y) = player ^. entityBody . bodyPosition
        (RectangleBox w _) = player ^. entityBody . bodyCollisionBox
        directionMultiplier =
            case player ^. direction of
                LeftDirection -> -1
                RightDirection -> 1

        xOffset = directionMultiplier*w/2 + bulletOffset
        bulletPosition = (x + xOffset, y)

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