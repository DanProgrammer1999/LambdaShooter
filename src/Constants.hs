module Constants where

maxMovementSpeed :: Float
maxMovementSpeed = 1000000

accelerationRate :: Float
accelerationRate = 50000

jumpAcceleration :: Float
jumpAcceleration = 1000

defaultPosition :: (Float, Float)
defaultPosition = (0, 2000)

g :: Float
g = 9.81

entitiesScale :: Float
entitiesScale = 0.2

-------------------------------
blankAnimationPath :: FilePath
blankAnimationPath = "resources/blank"

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


