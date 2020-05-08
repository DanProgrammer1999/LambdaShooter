module Demo  where

import CommonData
import Graphics.Gloss

sampleWorld ::  World
sampleWorld  = World
 (Map (color green $ circleSolid 100) 200 300 [])
 []
 (PlayerData [] 0 100 0 "Alex")
