module Constants where

import Graphics.Gloss

maxMovementSpeed :: Float
maxMovementSpeed = 1000000

accelerationRate :: Float
accelerationRate = 10000

jumpAcceleration :: Float
jumpAcceleration = 1000

defaultPosition :: (Float, Float)
defaultPosition = (0, 100)

defaultVelocity :: (Float, Float)
defaultVelocity = (0, 0)

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

----- Player sprites  -----
terroristIdlePath :: FilePath
terroristIdlePath  = "resources/pinguin/rifle/idle"

terroristRunPath :: FilePath
terroristRunPath   = "resources/pinguin/rifle/run"

terroristDeathPath :: FilePath
terroristDeathPath = "resources/pinguin/rifle/death"

terroristJumpPath :: FilePath
terroristJumpPath  = "resources/pinguin/rifle/jump"

terroristFallPath :: FilePath
terroristFallPath  = "resources/pinguin/rifle/fall"


----- Pictures  -----
getDefaultPicture :: Picture
getDefaultPicture = color yellow $ rectangleSolid 100 100

getBodyColor :: Color
getBodyColor = blue


