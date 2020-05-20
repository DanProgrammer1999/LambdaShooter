{-# LANGUAGE TemplateHaskell #-}

module CommonData where

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Picture
import Graphics.Gloss
import Control.Lens
import Data.Maybe

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
    deriving Show
makeLenses ''Weapon

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
makeLenses ''CollisionBox

data Body = Body
    { _bodyPosition     :: Position
    , _bodyVelocity     :: Velocity
    , _weight           :: Float
    , _bodyCollisionBox :: CollisionBox
    , _collisionHappened :: Bool
    } deriving Show
makeLenses ''Body

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
makeLenses ''EntityData

data Entity
    = Entity
    { _entityBody    :: Body
    , _entityTexture :: Picture -- ^ unused (Blank) for players.
    , _entityData    :: EntityData
    , _direction     :: Direction
    }
makeLenses ''Entity

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
    { _worldMap     :: Map
    , _projectiles  :: [Entity]
    , _players      :: [Entity]
    , _myPlayer     :: Entity
    , _keyboardData :: KeyboardInfo
    , _shootingCooldown :: Float
    }
makeLenses ''World

instance Show Entity where 
    show e@(Entity body _ eData direction) 
        | isPlayer e = 
            show body ++ 
            "; State:" ++ show (_currentState eData) ++ 
            "; Direction: " ++ show direction ++ 
            "; AnimationInfo: " ++ show 
                (fromMaybe getDefaultAnimation (getAnimationFromEntity e))
        | otherwise = 
            show body ++
            "; Direction: " ++ show direction ++ "\n"

makeBullet :: Float -> Position -> Direction -> Entity
makeBullet bulletPower origin direction = Entity body texture (ProjectileData bulletPower) direction
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