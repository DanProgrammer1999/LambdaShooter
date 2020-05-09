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
    , _hasJumped     :: Bool
    }

data Entity
    = Laser  { _laserBody  :: Body }
    | Bullet { _bulletBody :: Body }

data Player = Player
    { _playerBody :: Body
    , _playerData :: PlayerData
    }

data Body = Body
    { _bodyPosition     :: Position
    , _bodyVelocity     :: Velocity
    , _bodyAcceleration :: Acceleration
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
    , _players    :: [Player]
    , _myPlayer   :: Player
    }

makeLenses ''PlayerData
makeLenses ''Body
makeLenses ''Player
makeLenses ''Entity
makeLenses ''Map
makeLenses ''Block
makeLenses ''World
