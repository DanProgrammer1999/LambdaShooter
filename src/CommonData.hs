{-# LANGUAGE TemplateHaskell #-}

module CommonData where

import Graphics.Gloss.Data.Point
import Control.Lens

data Weapon
    = Blaster Double
    | Sword Double
    | Gun Integer

type Position = Point
type Velocity = Point
type Acceleration = Point

data PlayerData = PlayerData
    { _weapons       :: [Weapon]
    , _choosenWeapon :: Integer
    , _health        :: Double
    , _score         :: Double
    , _name          :: String
    }

data EntityData
    = Laser
    | Bullet
    | Player {_playerData :: PlayerData}
    -- is sword entity? how would it move? 

data Entity = Entity
    { _entityData         :: EntityData
    , _entityPosition     :: Position
    , _entityVelocity     :: Velocity
    , _entityAcceleration :: Acceleration
    -- do we need rotation, rotationSpeed and rotationAcceleration (and maybe pivotPoint)?
    }

data Block = Block { _blockPosition :: Position }

data Map = Map
    { _maxWidth  :: Double
    , _maxHeight :: Double
    , _tiles     :: [Block]
    }

data World = World
    { _worldMap   :: Map
    , _entities   :: [Entity]
    , _myPlayer   :: Entity
    }

makeLenses ''PlayerData
makeLenses ''EntityData
makeLenses ''Entity
makeLenses ''Map
makeLenses ''Block
makeLenses ''World
