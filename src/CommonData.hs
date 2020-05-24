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

import Constants

type Position = Point
type Velocity = Point
type Name     = String
type ID       = Int
type Health   = Float
type Score    = Float

data PlayerState 
    = Idle 
    | Running 
    | Jumping 
    | Falling 
    | Dying 
    | Shooting 
    | EmptyState 
    deriving (Generic, Eq, Show) 

instance ToJSON   PlayerState
instance FromJSON PlayerState

data Direction =  LeftDirection | RightDirection deriving (Generic, Eq, Show)
instance ToJSON   Direction
instance FromJSON Direction


-- | Note: we can swich to array for O(1) index-based access.
-- That would be useful since we always access frames by index.
data Animation = Animation
  { _frameDelay    :: Float      -- ^ How long to wait between frames
  , _frames        :: [Picture]  -- ^ All frames 
  , _flippedFrames :: [Picture]  -- ^ Flipped frames (for Left Direction actions)
  , _waitFor       :: Float      -- ^ Time until next frame
  , _curFrame      :: Int        -- ^ Current number of frame
  , _isOnce        :: Bool       -- ^ Should the animation by cyclic or played once?
  }
makeLenses ''Animation

instance Show Animation where
  show (Animation _ frames _ _ curFrame _) =
    " frames length: " ++ show (length frames) ++
    "; CurFrame: " ++ show curFrame

type PlayerAnimationTable = [(PlayerState, Animation)]

data GameGraphics
    = GameGraphics 
    { _playerAnimations   :: PlayerAnimationTable
    , _backgroundPicture  :: Picture
    , _bulletPicture      :: Picture
    , _blockPicture       :: Picture
    } 
makeLenses ''GameGraphics

data CollisionBox
    = RectangleBox
        { _width  :: Float
        , _height :: Float
        } 
    | CircleBox
        {_radius :: Float } 
    deriving (Generic, Show)
makeLenses ''CollisionBox

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
    { _deaths        :: Int
    , _kills         :: Int
    , _level         :: Int
    , _lastDeathTime :: Int
    }
    deriving (Generic, Show)
makeLenses ''PlayerStatistics
instance ToJSON   PlayerStatistics
instance FromJSON PlayerStatistics

data EntityData
    = PlayerData
    { _health        :: Health
    , _name          :: Name
    , _statistics    :: PlayerStatistics
    , _currentState  :: PlayerState
    } 
    | ProjectileData
    { _projectilePower :: Float
    , _ownerID         :: ID
     } deriving (Generic, Show)
makeLenses ''EntityData

instance ToJSON   EntityData
instance FromJSON EntityData

data Entity
    = Entity
    { _entityID      :: ID
    , _entityBody    :: Body
    , _entityData    :: EntityData
    , _direction     :: Direction
    } deriving (Generic, Show)
makeLenses ''Entity


data Block = Block
    { _blockPosition :: Position
    , _blockWidth    :: Float
    , _blockHeight   :: Float
    } deriving (Generic, Show)
makeLenses ''Block

instance ToJSON   Block
instance FromJSON Block

data Map = Map
    { _blocks :: [Block]} 
    deriving (Generic, Show)
makeLenses ''Map

instance ToJSON   Map
instance FromJSON Map

data KeyboardInfo = KeyboardInfo
    { _rightKeyPressed   :: Bool
    , _leftKeyPressed    :: Bool
    , _jumpKeyPressed    :: Bool
    , _fireKeyPressed    :: Bool
    } deriving (Generic, Show)
makeLenses ''KeyboardInfo

instance ToJSON   KeyboardInfo
instance FromJSON KeyboardInfo

data World = World
    { _worldMap         :: Map
    , _projectiles      :: [Entity]
    , _myProjectiles    :: [Entity]
    , _players          :: [Entity]
    , _myPlayer         :: Entity
    , _keyboardData     :: KeyboardInfo
    , _shootingCooldown :: Float
    } deriving (Generic, Show)
makeLenses ''World

data Universe = Universe
    {_world :: World
    ,_graphics :: GameGraphics   
    }

instance Eq Entity where
    (==) e1 e2 = e1 ^. entityID == e2 ^.entityID

-- instance Show Entity where 
--     show e@(Entity id body eData direction) 
--         | isPlayer e = 
--             show body ++ 
--             "; State:" ++ show (_currentState eData) ++ 
--             "; Health:" ++ show (fromMaybe 0 (eData ^? health)) ++
--             "; Direction: " ++ show direction
--         | otherwise = 
--             show body ++
--             "; Direction: " ++ show direction ++ "\n"
instance ToJSON   Entity
instance FromJSON Entity

instance ToJSON   World
instance FromJSON World

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


