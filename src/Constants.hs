module Constants where

import Graphics.Gloss

playerWeight :: Float
playerWeight = 10

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
penguinIdlePath :: FilePath
penguinIdlePath  = "resources/penguin/idle"

penguinRunPath :: FilePath
penguinRunPath   = "resources/penguin/run"

penguinDeathPath :: FilePath
penguinDeathPath = "resources/penguin/death"

penguinJumpPath :: FilePath
penguinJumpPath  = "resources/penguin/jump"

penguinFallPath :: FilePath
penguinFallPath  = "resources/penguin/fall"

penguinShootingPath :: FilePath
penguinShootingPath = "resources/penguin/idle_shooting"


----- Pictures  -----
getDefaultPicture :: Picture
getDefaultPicture = color yellow $ rectangleSolid 100 100

getBodyColor :: Color
getBodyColor = blue


