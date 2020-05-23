module Constructors 
    ( getDefaultPicture
    , makeBullet
    , makePlayer
    , clientInfoFromEntity
    , entityFromClientInfo
    , keyboardInfo
    , playerStatistics
    ) where

import Control.Lens
import Data.Maybe

import Graphics.Gloss

import CommonData
import Constants

getDefaultPicture :: Picture
getDefaultPicture = color yellow $ rectangleSolid 100 100

makeBullet :: Float -> Position -> Direction -> Entity
makeBullet bulletPower origin direction = Entity bulletID body texture (ProjectileData bulletPower) direction
    where
        velocity = defaultBulletVelocity & _1 *~ (if direction == LeftDirection then -1 else 1)
        body = Body origin velocity bulletWeight bulletCollisionBox False
        texture = color black $ circleSolid 20

bulletCollisionBox :: CollisionBox
bulletCollisionBox = CircleBox 5

-- | Default id for bullets.
bulletID :: Int
bulletID = 0

makePlayer :: ID -> Name -> [(PlayerState, Animation)] -> Entity
makePlayer uniqueID name animationTable =
     Entity uniqueID playerBody Blank playerData RightDirection where
        playerData = PlayerData maxHealth name playerStatistics Idle animationTable 
        playerBody = Body playerSpawnPosition defaultVelocity playerWeight playerCollisionBox False

playerCollisionBox :: CollisionBox
playerCollisionBox = RectangleBox 40 80

playerStatistics :: PlayerStatistics
playerStatistics = Statistics 0 0 0

clientInfoFromEntity :: Entity -> ClientInfo
clientInfoFromEntity e = ClientInfo 
    { _clientID            = e ^. entityID
    , _clientName          = e ^. entityData . name
    , _clientBody          = e ^. entityBody
    , _clientState         = fromMaybe EmptyState $ e ^? entityData . currentState
    , _clientDirection     = e ^. direction
    , _clientHealth        = _health $ e ^. entityData
    , _clientStatistics    = fromMaybe playerStatistics $ e ^? entityData . statistics
    }

entityFromClientInfo :: PlayerAnimationTable -> ClientInfo -> Entity
entityFromClientInfo table info = Entity id body Blank edata direction 
    where
        edata = PlayerData 
            { _health        = info ^. clientHealth
            , _statistics    = info ^. clientStatistics
            , _name          = info ^. clientName
            , _currentState  = info ^. clientState
            , _animations    = table
            } 
        body      = info ^. clientBody
        direction = info ^. clientDirection
        id        = info ^. clientID     

keyboardInfo :: KeyboardInfo
keyboardInfo = KeyboardInfo False False False False
