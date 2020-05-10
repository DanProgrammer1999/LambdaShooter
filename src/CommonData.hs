{-# LANGUAGE TemplateHaskell #-}

module CommonData where

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Picture
import Control.Lens
import Data.Maybe
import Animation

import Textures

data Weapon
    = ShootingWeapon
        { _shootingWeaponTexture :: TextureName
        , _shootingPower         :: Float
        }
    | ColdWeapon
        { _coldWeaponTexture :: TextureName
        , _cutPower          :: Float
        , _cutRadius         :: Float
        }
    | WaveWeapon
        { _waveWeaponTexture :: TextureName
        , _wavePower         :: Float
        , _waveHitRadius     :: Float
        }

type Position = Point
type Velocity = Point
type Acceleration = Point

data CollisionBox
    = RectangleBox 
        { _width  :: Float
        , _height :: Float
        }
    | CircleBox    
        {_radius :: Float }

data Body = Body
    { _bodyPosition     :: Position
    , _bodyVelocity     :: Velocity
    , _weight           :: Float
    , _bodyAcceleration :: Acceleration
    , _bodyCollisionBox :: CollisionBox
    -- do we need rotation, rotationSpeed and rotationAcceleration (and maybe pivotPoint)?
    }

data PlayerState = Idle | Running | Jumping | Falling deriving Eq 
data Direction =  Left | Right deriving Eq

data EntityData
    = PlayerData
    { _weapons       :: [Weapon]
    , _choosenWeapon :: Integer
    , _health        :: Float
    , _score         :: Float
    , _name          :: String
    , _hasJumped     :: Bool
    , _currentState  :: PlayerState
    , _direction     :: Direction
    , _animations    :: [(PlayerState, Animation)]
    }
    | ProjectileData
    { _projectilePower :: Float }

data Entity
    = Entity
    { _entityBody    :: Body
    , _entityTexture :: Picture -- ^ unused for players.
    , _entityData    :: EntityData
    }

data Block = Block
    { _blockPosition :: Position
    , _blockTexture  :: Picture
    , _blockWidth    :: Float
    , _blockHeight   :: Float
    }

data Map = Map
    { _background :: Picture
    , _maxWidth   :: Float
    , _maxHeight  :: Float
    , _blocks      :: [Block]
    }

data World = World
    { _worldMap   :: Map
    , _entities   :: [Entity]
    , _myPlayer   :: Entity
    }

makeLenses ''Weapon
makeLenses ''CollisionBox
makeLenses ''Body
makeLenses ''EntityData
makeLenses ''Entity
makeLenses ''Block
makeLenses ''Map
makeLenses ''World

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

addPoints :: Point -> Point -> Point
addPoints (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt $ (x1 - x2)^2 + (y1 - y2)^2