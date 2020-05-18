{-# LANGUAGE TemplateHaskell #-}

module CommonData where

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Picture
import Control.Lens
import Data.Maybe

import Animation

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
    deriving Show

type Position = Point
type Velocity = Point

data CollisionBox
    = RectangleBox
        { _width  :: Float
        , _height :: Float
        } 
    | CircleBox
        {_radius :: Float } 
    deriving Show

data Body = Body
    { _bodyPosition     :: Position
    , _bodyVelocity     :: Velocity
    , _weight           :: Float
    , _bodyCollisionBox :: CollisionBox
    } deriving Show

data EntityData
    = PlayerData
    { _weapons       :: [Weapon]
    , _choosenWeapon :: Integer
    , _health        :: Float
    , _score         :: Float
    , _name          :: String
    , _currentState  :: PlayerState
    , _animations    :: [(PlayerState, Animation)]
    }
    | ProjectileData
    { _projectilePower :: Float }

data Entity
    = Entity
    { _entityBody    :: Body
    , _entityTexture :: Picture -- ^ unused (Blank) for players.
    , _entityData    :: EntityData
    , _direction     :: Direction
    }
instance Show Entity where 
    show (Entity body _ eData direction) = 
        show body ++ "; State:" ++ show (_currentState eData) ++ "; Direction: " ++ show direction

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
    , _blocks     :: [Block]
    }

data KeyboardInfo = KeyboardInfo
    { _rightKeyPressed   :: Bool
    , _leftKeyPressed    :: Bool
    , _jumpKeyPressed    :: Bool
    , _fireButtonPressed :: Bool
    }

keyboardInfo :: KeyboardInfo
keyboardInfo = KeyboardInfo False False False False

data World = World
    { _worldMap     :: Map
    , _entities     :: [Entity]
    , _myPlayer     :: Entity
    , _keyboardData :: KeyboardInfo
    }

makeLenses ''Weapon
makeLenses ''CollisionBox
makeLenses ''Body
makeLenses ''EntityData
makeLenses ''Entity
makeLenses ''Block
makeLenses ''Map
makeLenses ''KeyboardInfo
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

-- | Returns the right animation from the entitie's animation table
getAnimationFromEntity :: Entity -> Maybe Animation
getAnimationFromEntity entity = animation where
    animationTable = entity ^. entityData . animations
    state = fromMaybe Idle (entity ^? entityData . currentState)
    animation = lookup state animationTable
