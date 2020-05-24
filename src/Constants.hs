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

fallAcceleration :: Float
fallAcceleration = 5

defaultVelocity :: (Float, Float)
defaultVelocity = (0, 0)

-- | Velocity multiplied by this on collision
-- | Used to avoid too abrupt velocity cut on fall
collisionVelocityRate :: Float
collisionVelocityRate = 0.15

----- Player -----

playerWeight :: Float
playerWeight = 7.5

accelerationRate :: Float
accelerationRate = 250

jumpAcceleration :: Float
jumpAcceleration = 1000

defaultPosition :: (Float, Float)
defaultPosition = (0,0)

defaultHP :: Float
defaultHP = 100

-- | Very small velocity which is considered 0
stopVelocity :: Float 
stopVelocity = 1

maxHealth :: Float 
maxHealth = 100

playerSpawnPosition :: (Float, Float)
playerSpawnPosition = (0, 100)

-------- Logic --------

killsToLevelUp :: [Int]
killsToLevelUp = [1, 5, 10, 20, 35, 50, 75, 100]

----- Projectiles -----

bulletWeight :: Float
bulletWeight = 0.1

bulletOffset :: Float
bulletOffset = 10

defaultBulletVelocity :: (Float, Float)
defaultBulletVelocity = (50, 0)

baseBulletPower :: Float
baseBulletPower = 15

-- in seconds
maxShootingCooldown :: Float 
maxShootingCooldown = 0.33

----- World -----

worldBoundary :: Float
worldBoundary = 10000

blockTextureSize :: (Float, Float)
blockTextureSize = (210, 68)

----- Rendering -----

defaultFrameDelay :: Float
defaultFrameDelay = 0.02

entitiesScale :: Float
entitiesScale = 0.2

playerPictureWidth :: Float
playerPictureWidth = 420

playerPictureHeight :: Float
playerPictureHeight = 480

nameYOffset :: Float
nameYOffset = 170

healthbarYOffset :: Float
healthbarYOffset = 0

healthBarLength :: Float 
healthBarLength = playerPictureWidth + 40

defaultWindowSize :: (Int, Int)
defaultWindowSize = (1280, 720)

-------------------------------

backgroundPath :: FilePath
backgroundPath = "resources/background.png"

blockTexturePath :: FilePath
blockTexturePath = "resources/block.png"

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

getBodyColor :: Color
getBodyColor = blue
