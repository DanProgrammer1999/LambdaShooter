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

-- ^ Receive events and update the world
handleInput :: Event -> World -> World
handleInput (EventKey (Char c) Down _ _) world = world & myPlayer .~ newPlayer
    where
        accelerationLens = entityBody . bodyAcceleration
        currPlayer = world ^. myPlayer
        c' = toLower c
        newPlayer =
            case c' of
                'a' -> currPlayer & accelerationLens . _1 -~ accelerationRate
                'd' -> currPlayer & accelerationLens . _1 +~ accelerationRate
                ' ' -> decelerate (makeJump currPlayer)
                _   -> decelerate currPlayer

handleInput _ world = world

decelerate :: Entity -> Entity
decelerate player = player & entityBody . bodyAcceleration . _1 -~ deceleration
    where
        currAcceleration = player ^. entityBody . bodyAcceleration . _1
        deceleration = signum currAcceleration * accelerationRate

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



-- ^ Update entities parameters (position, velocity, acceleration) based on time passed
-- ^ Gravity calculations and collision detection is also here
-- ^ Note: first update events, physics, then animation
updateWorld :: Float -> World -> World
updateWorld timePassed world = newWorld
    where
        gravity_acceleration body = body ^. weight * g

        newWorld = world & myPlayer %~ updateEntity
        
        updateEntity :: Entity -> Entity
        updateEntity entity
            = entity &~ do
                entityBody . bodyPosition .= newPosition
                entityBody . bodyVelocity .= newVelocity

                entityData . animations .= newPlayerTable
            where
                oldPosition = entity ^. entityBody . bodyPosition
                oldVelocity = entity ^. entityBody . bodyVelocity
                oldAcceleration = entity ^. entityBody . bodyAcceleration

                newVelocity = limitVelocity $ addPoints oldVelocity oldAcceleration
                newPosition = addPoints newVelocity oldPosition


                -- ^ All code below should be applied to players only

                -- ^ Update Animation should be done for all players
                curState = _currentState (entity ^. entityData)
                -- ^ Calculate new Animation
                oldPlayerAnimation = fromJust $ getAnimationFromEntity entity
                newPlayerAnimation = updateAnimation timePassed oldPlayerAnimation
                -- ^ Calculate new Animation Table
                oldPlayerTable  = entity ^. entityData . animations
                newPlayerTable = (curState, newPlayerAnimation)
                    : filter (\(state, _) -> state == curState) oldPlayerTable

-- ^ Calculate if the movement would cause collision
-- ^ If it does, return the point to which the body can move
-- ^ and the entity with which the body has been collided
tryMove :: World -> Entity -> Position -> (Position, Maybe Entity)
tryMove world entity (x, y) = ((x, y), Nothing)

-- checkMapCollision :: Entity -> Map -> Bool
-- checkMapCollision entity (Map _ maxMapWidth maxMapHeight blocks) 
--     = _
--     where
--         blockPositions = 

limitVelocity :: Velocity -> Velocity
limitVelocity (x, y) = (newX, newY)
    where
        newX = max x maxMovementSpeed
        newY = max y maxMovementSpeed

checkEntityCollision :: Entity -> Entity -> Bool
checkEntityCollision entity1 entity2 = detectCollision position1 position2 box1 box2
    where
        position1 = entity1 ^. entityBody . bodyPosition
        position2 = entity2 ^. entityBody . bodyPosition

        box1 = entity1 ^. entityBody . bodyCollisionBox
        box2 = entity2 ^. entityBody . bodyCollisionBox

detectCollision :: Position -> Position -> CollisionBox -> CollisionBox -> Bool
detectCollision
    (x1, y1)
    (x2, y2)
    (RectangleBox width1 height1)
    (RectangleBox width2 height2)
    =  x1 < x2 + width2
    && x1 + width1 > x2
    && y1 < y2 + height2
    && y1 + height1 > y2

detectCollision c1 c2 (CircleBox r1) (CircleBox r2)
    = distance c1 c2 < r1 + r2

detectCollision
    (xr, yr)
    (xc, yc)
    (RectangleBox width height)
    (CircleBox r)
    = distance (testX, testY) (xc, yc) <= r
    where
        testX = if xc < xr then xr else xr + width
        testY = if yc < yr then yr else yr + height

detectCollision p1 p2 c@(CircleBox _) r@(RectangleBox _ _)
    = detectCollision p2 p1 r c
