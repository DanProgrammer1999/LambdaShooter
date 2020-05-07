module CommonData where

data Weapon 
    = Blaster Double
    | Sword Double
    | Gun Integer 

newtype EntityPosition = EntityPosition { entityPosition :: (Double, Double) }
newtype BlockPosition = BlockPosition { tilePosition :: (Double, Double) }

data Player = Player 
    { weapons  :: [Weapon]
    , health   :: Double
    , score    :: Double
    , name     :: String
    , entity   :: WorldEntity
    } 

data Entity 
    = Laser 
    | Bullet 
    -- is sword entity? how would it move? 

data WorldEntity = WorldEntity
    { entity       :: Entity
    , position     :: EntityPosition
    , speed        :: (Double, Double)
    , acceleration :: (Double, Double)
    -- do we need rotation, rotationSpeed and rotationAcceleration (and maybe pivotPoint)?
    }

data Block = Block { position :: BlockPosition }

data Map = Map
    { maxWidth  :: Double
    , maxHeight :: Double
    , tiles     :: [Block]
    }

data World = World 
    { map        :: Map
    , entities   :: [WorldEntity]
    , players    :: [Player]
    , myUsername :: String
    }
