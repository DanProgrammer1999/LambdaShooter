module Constants where

maxMovementSpeed :: Float
maxMovementSpeed = 500

accelerationRate :: Float
accelerationRate = 100

decelerationRate :: Float
decelerationRate = accelerationRate

frictionRate :: Float
frictionRate = accelerationRate

jumpAcceleration :: Float
jumpAcceleration = 2.0

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


