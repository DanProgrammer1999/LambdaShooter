module CommonData where

data Weapon 
    = Blaster Double
    | Sword Double
    | Gun Integer 

type Position = (Double, Double)
type Velocity = (Double, Double)
type Acceleration = (Double, Double)

data PlayerData = PlayerData
    { weapons       :: [Weapon]
    , choosenWeapon :: Integer 
    , health        :: Double
    , score         :: Double
    , name          :: String
    } 

data EntityData
    = Laser 
    | Bullet 
    | Player PlayerData
    -- is sword entity? how would it move? 

data Entity = Entity
    { entityData         :: EntityData
    , entityPosition     :: Position
    , entityVelocity     :: Velocity
    , entityAcceleration :: Acceleration
    -- do we need rotation, rotationSpeed and rotationAcceleration (and maybe pivotPoint)?
    }

data Block = Block { blockPosition :: Position }

data Map = Map
    { maxWidth  :: Double
    , maxHeight :: Double
    , tiles     :: [Block]
    }

data World = World 
    { map        :: Map
    , entities   :: [Entity]
    , myPlayer   :: Entity
    }
