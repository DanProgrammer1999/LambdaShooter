module CommonData where

data Weapon 
    = Blaster Double
    | Sword Double
    | Gun Integer 

newtype Position = Position { position :: (Double, Double) }
newtype Velocity = Velocity { velocity :: (Double, Double) }
newtype Acceleration = Acceleration { acceleration :: (Double, Double) }

data Player = Player 
    { weapons  :: [Weapon]
    , health   :: Double
    , score    :: Double
    , name     :: String
    , body     :: Entity
    } 

data EntityType
    = Laser 
    | Bullet 
    | Player
    -- is sword entity? how would it move? 

data Entity = Entity
    { entityType         :: EntityType
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
    , players    :: [Player]
    , myUsername :: String
    }