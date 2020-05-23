module Constructors 
    ( getDefaultPicture
    , makeBullet
    , makePlayer
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

makeBullet :: Float -> Position -> Direction -> ID -> Entity
makeBullet bulletPower origin direction ownerID =
     Entity bulletID body (ProjectileData bulletPower ownerID) direction
    where
        velocity = defaultBulletVelocity & _1 *~ (if direction == LeftDirection then -1 else 1)
        body = Body origin velocity bulletWeight bulletCollisionBox False

bulletCollisionBox :: CollisionBox
bulletCollisionBox = CircleBox 5

-- | Default id for bullets.
bulletID :: Int
bulletID = 0

makePlayer :: ID -> Name -> Entity
makePlayer uniqueID name  =
     Entity uniqueID playerBody playerData RightDirection where
        playerData = PlayerData maxHealth name playerStatistics Idle 
        playerBody = Body playerSpawnPosition defaultVelocity playerWeight playerCollisionBox False

playerCollisionBox :: CollisionBox
playerCollisionBox = RectangleBox 40 80

playerStatistics :: PlayerStatistics
playerStatistics = Statistics 0 0 0

keyboardInfo :: KeyboardInfo
keyboardInfo = KeyboardInfo False False False False
