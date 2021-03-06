module Logic where

import Control.Lens
import Data.Maybe
import Data.Either
import Data.Char

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector

import CommonData
import Constants
import Constructors (makeBullet, playerStatistics)
import Animation
import Physics

-- | Receive events and update the world
handleInput :: Event -> Universe -> Universe
handleInput (EventKey (Char c) state _ _) (Universe world graphic)
    = Universe newWorld graphic where
        newWorld = world & keyboardData %~ keyAction (toLower c) (state == Down)
handleInput (EventKey (SpecialKey KeySpace) state _ _) (Universe world graphic)
    = Universe newWorld graphic where
         newWorld = world & keyboardData %~ keyAction ' ' (state == Down)
handleInput (EventKey (MouseButton LeftButton) state _ _) (Universe world graphic)
    = Universe newWorld graphic where
         newWorld = world & keyboardData . fireKeyPressed .~ (state == Down)

handleInput _ u = u

keyAction :: Char -> Bool -> KeyboardInfo -> KeyboardInfo
keyAction 'a' isDown info = info & leftKeyPressed .~ isDown
keyAction 'd' isDown info = info & rightKeyPressed .~ isDown
keyAction ' ' isDown info = info & jumpKeyPressed .~ isDown
keyAction _ _        info = info

updateWorld ::Float -> Universe -> Universe
updateWorld timePassed universe@(Universe world graphics) =
    Universe (withNewPlayer & projectiles .~ filteredProjectiles) newGraphics
    where
        (Universe withNewPlayer newGraphics) = updateMyPlayer timePassed universe
        oldProjectiles = withNewPlayer ^. projectiles
        updateProjectile = over entityBody $ updateBody timePassed world
        updatedProjectiles = updateProjectile <$> oldProjectiles

        -- delete projectiles with collision or out of world
        projectileFilter projectile 
            =  not (projectile ^. entityBody . collisionHappened)
            && not (isOutOfBounds (projectile ^. entityBody))
        filteredProjectiles = filter projectileFilter updatedProjectiles

updateMyPlayer :: Float -> Universe -> Universe
updateMyPlayer  timePassed u@(Universe world graphics) = Universe newWorld newGraphics
    where
        newWorld = world &~ do
            myPlayer .= newPlayer
            myPlayer . entityData . currentState .= newState
            myPlayer . direction %= getNewDirection keyboard

            myProjectiles %=
                (\oldProjectiles ->
                    if willShoot then createBullet newPlayer : oldProjectiles else oldProjectiles
                )
            shootingCooldown %=
                (\cooldown -> if willShoot then maxShootingCooldown else max 0 (cooldown - timePassed))
        newGraphics = graphics & playerAnimations .~ newAnimations
        keyboard = world ^. keyboardData
        oldPlayer = world ^. myPlayer

        oldVelocity = oldPlayer ^. entityBody . bodyVelocity
        deltaVelocity = mulSV timePassed $ getNewVelocity keyboard oldPlayer
        newVelocity = deltaVelocity & _2 +~ snd oldVelocity

        newPlayer = oldPlayer &~ do
            entityBody . bodyVelocity .= newVelocity
            entityBody %= updateBody timePassed world

        oldState = fromMaybe EmptyState $ oldPlayer ^? entityData . currentState
        willShoot = canShoot && requestedShoot
            where
                requestedShoot = world ^. keyboardData . fireKeyPressed
                canShoot = world ^. shootingCooldown == 0 && (oldState /= Dying) 
        
        newState = if willShoot then Shooting else getNewState newPlayer
        newAnimations = getNewPlayerAnimations graphics timePassed newPlayer (newState /= oldState)

damagePlayer :: Entity -> Float -> Entity
damagePlayer entity@(Entity id body eData dir) dmg 
    = entity & entityData .~ eDataUpdated
    where
        newHealth = fromMaybe 0 (eData ^? health) - dmg
        oldState = fromMaybe Idle $ eData ^? currentState
        newState = if newHealth <= 0 then Dying else oldState
        eDataUpdated = eData &~ do
            health       .= newHealth
            currentState .= newState

        oldStatistics = fromMaybe playerStatistics $ eData ^? statistics
        oldDeaths     = oldStatistics ^. deaths

killIfOutOfWorld :: Entity -> Entity
killIfOutOfWorld player 
    | abs x > worldBoundary 
        || abs y > worldBoundary = damagePlayer player (maxHealth + 1) 
    | otherwise = player
    where 
        (x, y) = player ^. entityBody . bodyPosition

createBullet :: Entity -> Entity
createBullet player = makeBullet bulletPower bulletPosition (player ^. direction) playerID
    where
        playerID = player ^. entityID
        (x, y) = player ^. entityBody . bodyPosition
        playerLevel = player ^? entityData . statistics . level & fromMaybe 0
        (RectangleBox w _) = player ^. entityBody . bodyCollisionBox
        directionMultiplier =
            case player ^. direction of
                LeftDirection -> -1
                RightDirection -> 1

        xOffset = directionMultiplier * w/2 + bulletOffset
        bulletPosition = (x + xOffset, y)
        bulletPower = baseBulletPower * (1 + fromIntegral playerLevel * 0.1)

getNewState :: Entity -> PlayerState
getNewState entity
    | currState == Just Dying          = Dying
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
    
calculateLevel :: PlayerStatistics -> Int
calculateLevel (Statistics deaths kills _ _) = max 0 newLevel
    where 
        findLevel idx [] = 0
        findLevel idx (x : xs)
            | kills >= x  = idx
            | otherwise  = findLevel (idx + 1) xs
        
        newLevel = findLevel 0 killsToLevelUp - deaths



getNewPlayerAnimations :: GameGraphics -> Float -> Entity -> Bool -> PlayerAnimationTable
getNewPlayerAnimations graphics timePassed player wasStateChange = newPlayerTable
    where
        -- | Update Animation should be done for all players
        curState = fromMaybe EmptyState (player ^? entityData . currentState)
        -- | Calculate new Animation
        oldPlayerAnimation = fromMaybe getDefaultAnimation
            $ getAnimationFromEntity graphics (player ^. entityData)
        newPlayerAnimation = newAnimation where
            anim = updateAnimation timePassed oldPlayerAnimation
            newAnimation = if wasStateChange then anim {_curFrame = 0} else anim
        -- | Calculate new Animation Table
        oldPlayerTable = graphics ^. playerAnimations
        newPlayerTable = (curState, newPlayerAnimation)
            : filter (\(state, _) -> state /= curState) oldPlayerTable
