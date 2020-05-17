module Constants where

import Graphics.Gloss

maxMovementSpeed :: Float
maxMovementSpeed = 1000000

accelerationRate :: Float
accelerationRate = 10000

jumpAcceleration :: Float
jumpAcceleration = 1000

defaultPosition :: (Float, Float)
defaultPosition = (1000, 100)

-- | Very small velocity which is considered 0
stopVelocity :: Float 
stopVelocity = 15

-- | Velocity multiplied by this on collision
-- | Used to avoid too abrupt velocity cut on fall
collisionVelocityRate :: Float
collisionVelocityRate = 0.1

g :: Float
g = 9.81

entitiesScale :: Float
entitiesScale = 0.2

-------------------------------
backgroundPath :: FilePath
backgroundPath = "resources/background.png"

----- Terrorist (Player)  -----
terroristIdlePath :: FilePath
terroristIdlePath = "resources/terrorist1/idle"

terroristRunPath :: FilePath
terroristRunPath = "resources/terrorist1/run"

terroristDeathPath :: FilePath
terroristDeathPath = "resources/terrorist1/death"

terroristJumpPath :: FilePath
terroristJumpPath = "resources/terrorist1/jump"

terroristFallPath :: FilePath
terroristFallPath = "resources/terrorist1/fall"


----- Pictures  -----
getDefaultPicture :: Picture
getDefaultPicture = color yellow $ rectangleSolid 100 100


