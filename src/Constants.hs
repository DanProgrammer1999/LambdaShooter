module Constants where

import Graphics.Gloss

----- Networking ------
defaultIP :: String
defaultIP  = "127.0.0.1"

defaultPort :: Int
defaultPort = 3579

-- | how often to sent data (for both client & server)
-- | 1000000 is 1 second.
delaySending :: Int
delaySending = 10000

----- Physics -----
simulationRate :: Int
simulationRate = 60

playerWeight :: Float
playerWeight = 7.5

accelerationRate :: Float
accelerationRate = 250

jumpAcceleration :: Float
jumpAcceleration = 1000

fallAcceleration :: Float
fallAcceleration = 5

defaultPosition :: (Float, Float)
defaultPosition = (0, 100)

defaultVelocity :: (Float, Float)
defaultVelocity = (0, 0)

maxHealth :: Float 
maxHealth = 100

-- | Very small velocity which is considered 0
stopVelocity :: Float 
stopVelocity = 1

-- | Velocity multiplied by this on collision
-- | Used to avoid too abrupt velocity cut on fall
collisionVelocityRate :: Float
collisionVelocityRate = 0.15

entitiesScale :: Float
entitiesScale = 0.2

-------------------------------

bulletWeight :: Float
bulletWeight = 0.1

bulletOffset :: Float
bulletOffset = 10

defaultBulletVelocity :: (Float, Float)
defaultBulletVelocity = (50, 0)

defaultBulletPower :: Float
defaultBulletPower = 35

-- in seconds
maxShootingCooldown :: Float 
maxShootingCooldown = 0.33

-------------------------------

worldBoundary :: Float
worldBoundary = 10000

-------------------------------

backgroundPath :: FilePath
backgroundPath = "resources/background.png"

----- Player sprites  -----

resourcePath :: FilePath 
resourcePath = "resources/penguin/cut/"

penguinIdlePath :: FilePath
penguinIdlePath  = resourcePath ++ "idle"

penguinRunPath :: FilePath
penguinRunPath   = resourcePath ++ "run"

penguinDeathPath :: FilePath
penguinDeathPath = resourcePath ++ "death"

penguinJumpPath :: FilePath
penguinJumpPath  = resourcePath ++ "jump"

penguinFallPath :: FilePath
penguinFallPath  = resourcePath ++ "fall"

penguinShootingPath :: FilePath
penguinShootingPath = resourcePath ++ "idle_shooting"


----- Pictures  -----
getDefaultPicture :: Picture
getDefaultPicture = color yellow $ rectangleSolid 100 100

getBodyColor :: Color
getBodyColor = blue


