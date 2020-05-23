{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module CommonData where

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Picture
import Control.Lens
import Graphics.Gloss
import Data.Maybe
import GHC.Generics
import Data.Aeson

import Animation
import Constants

data Weapon
    = ShootingWeapon
        { _shootingWeaponTexture :: Picture
        , _shootingPower         :: Float
        }
    | ColdWeapon
        { _coldWeaponTexture :: Picture
        , _cutPower          :: Float
        , _cutRadius         :: Float
        }
    | WaveWeapon
        { _waveWeaponTexture :: Picture
        , _wavePower         :: Float
        , _waveHitRadius     :: Float
        }
    deriving (Show)
makeLenses ''Weapon

type Position = Point
type Velocity = Point
type Name     = String
type ID       = Int
type Health   = Float
type Score    = Float
type ChoosenWeapon = Int

data CollisionBox
    = RectangleBox
        { _width  :: Float
        , _height :: Float
        } 
    | CircleBox
        {_radius :: Float } 
    deriving (Generic, Show)
makeLenses ''CollisionBox

defaultPlayerCollisionBox :: CollisionBox
defaultPlayerCollisionBox = RectangleBox 50 50

instance ToJSON   CollisionBox
instance FromJSON CollisionBox

data Body = Body
    { _bodyPosition     :: Position
    , _bodyVelocity     :: Velocity
    , _weight           :: Float
    , _bodyCollisionBox :: CollisionBox
    , _collisionHappened :: Bool
    } deriving (Generic, Show)
makeLenses ''Body
instance ToJSON   Body
instance FromJSON Body

data PlayerStatistics = Statistics
    { _deaths :: Integer
    , _kills  :: Integer
    }
    deriving (Generic, Show)
makeLenses ''PlayerStatistics
instance ToJSON   PlayerStatistics
instance FromJSON PlayerStatistics

data EntityData
    = PlayerData
    { _weapons       :: [Weapon]
    , _choosenWeapon :: ChoosenWeapon
    , _health        :: Health
    , _name          :: Name
    , _statistics    :: PlayerStatistics
    , _currentState  :: PlayerState
    , _animations    :: PlayerAnimationTable
    } 
    | ProjectileData
    { _projectilePower :: Float }
makeLenses ''EntityData

data Entity
    = Entity
    { _entityID      :: ID
    , _entityBody    :: Body
    , _entityTexture :: Picture -- ^ unused (Blank) for players.
    , _entityData    :: EntityData
    , _direction     :: Direction
    } deriving Generic
makeLenses ''Entity

-- | Default id for bullets.
bulletID :: Int
bulletID = 0

data Block = Block
    { _blockPosition :: Position
    , _blockTexture  :: Picture
    , _blockWidth    :: Float
    , _blockHeight   :: Float
    }
makeLenses ''Block

data Map = Map
    { _background :: Picture
    , _maxWidth   :: Float
    , _maxHeight  :: Float
    , _blocks     :: [Block]
    }
makeLenses ''Map

data KeyboardInfo = KeyboardInfo
    { _rightKeyPressed   :: Bool
    , _leftKeyPressed    :: Bool
    , _jumpKeyPressed    :: Bool
    , _fireKeyPressed    :: Bool
    }

keyboardInfo :: KeyboardInfo
keyboardInfo = KeyboardInfo False False False False
makeLenses ''KeyboardInfo

data World = World
    { _worldMap         :: Map
    , _projectiles      :: [Entity]
    , _players          :: [Entity]
    , _myPlayer         :: Entity
    , _keyboardData     :: KeyboardInfo
    , _shootingCooldown :: Float
    }
makeLenses ''World

data ClientInfo = ClientInfo
    { _clientID         :: ID
    , _clientName       :: Name
    , _clientBody       :: Body
    , _clientState      :: PlayerState
    , _clientDirection  :: Direction
    , _clientHealth     :: Health
    , _clientStatistics :: PlayerStatistics
    --, _clientWeapons   :: [Weapon]
    , _clientChoosenWeapon :: ChoosenWeapon
    } deriving (Generic, Show) 
makeLenses ''ClientInfo

instance ToJSON   ClientInfo
instance FromJSON ClientInfo

instance Eq ClientInfo where
    (==) ci1 ci2 = ci1 ^. clientID == ci2 ^.clientID

instance Eq Entity where
    (==) e1 e2 = e1 ^. entityID == e2 ^.entityID

instance Show Entity where 
    show e@(Entity id body _ eData direction) 
        | isPlayer e = 
            show body ++ 
            "; State:" ++ show (_currentState eData) ++ 
            "; Health:" ++ show (fromMaybe 0 (eData ^? health)) ++
            "; Direction: " ++ show direction ++ 
            "; AnimationInfo: " ++ show 
                (fromMaybe getDefaultAnimation (getAnimationFromEntity e))
        | otherwise = 
            show body ++
            "; Direction: " ++ show direction ++ "\n"

makeBullet :: Float -> Position -> Direction -> Entity
makeBullet bulletPower origin direction = Entity bulletID body texture (ProjectileData bulletPower) direction
    where
        velocity = defaultBulletVelocity & _1 *~ (if direction == LeftDirection then -1 else 1)
        body = Body origin velocity bulletWeight bulletCollisionBox False
        texture = color black $ circleSolid 5

bulletCollisionBox :: CollisionBox
bulletCollisionBox = CircleBox 5

isProjectile :: Entity -> Bool
isProjectile entity
    | isJust $ entity ^? testLens = True
    | otherwise                   = False
    where
        testLens = entityData . projectilePower

isPlayer :: Entity -> Bool
isPlayer entity
    | isJust $ entity ^? testLens = True
    | otherwise = False
    where
        testLens = entityData . name

-- | Returns the right animation from the entitie's animation table
getAnimationFromEntity :: Entity -> Maybe Animation
getAnimationFromEntity entity = animation where
    animationTable = entity ^. entityData . animations
    state = fromMaybe Idle (entity ^? entityData . currentState)
    animation = lookup state animationTable

clientInfoFromEntity :: Entity -> ClientInfo
clientInfoFromEntity e = ClientInfo 
    { _clientID            = e ^. entityID
    , _clientName          = e ^. entityData . name
    , _clientBody          = e ^. entityBody
    , _clientState         = fromMaybe EmptyState $ e ^? entityData . currentState
    , _clientDirection     = e ^. direction
    , _clientHealth        = _health $ e ^. entityData
    , _clientStatistics    = fromMaybe (Statistics 0 0) $ e ^? entityData . statistics
    --_clientWeapons      = e ^. entityData . weapons,
    , _clientChoosenWeapon = fromMaybe 0 $ e ^? entityData . choosenWeapon
    }

entityFromClientInfo :: PlayerAnimationTable -> ClientInfo -> Entity
entityFromClientInfo table info = Entity id body Blank edata direction where
    edata = PlayerData {
      _weapons       = [] -- TODO TOFIX problem with toJSON, fromJSON for Picture.
    , _choosenWeapon = info ^. clientChoosenWeapon
    , _health        = info ^. clientHealth
    , _statistics    = info ^. clientStatistics
    , _name          = info ^. clientName
    , _currentState  = info ^. clientState
    , _animations    = table
    } 
    body      = info ^. clientBody
    direction = info ^. clientDirection
    id        = info ^. clientID     
