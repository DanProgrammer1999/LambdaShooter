module Constants where

maxMovementSpeed :: Float
maxMovementSpeed = 1000000

accelerationRate :: Float
accelerationRate = 1000

decelerationRate :: Float
decelerationRate = accelerationRate

frictionRate :: Float
frictionRate = accelerationRate / 2

jumpHeight :: Float
jumpHeight = 2.0

g :: Float
g = 9.81

-------------------------------

entitiesScale :: Float
entitiesScale = 0.5

-------------------------------

backgroundPath :: FilePath
backgroundPath = "resources/background.png"

terroristIdlePath :: FilePath
terroristIdlePath = "resources/terrorist1/idle"

terroristRunPath :: FilePath
terroristRunPath = "resources/terrorist1/run"

terroristDeathPath :: FilePath
terroristDeathPath = "resources/terrorist1/death"

terroristJumpPath :: FilePath
terroristJumpPath = "resources/terrorist1/jump"