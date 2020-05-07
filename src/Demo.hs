module Demo (sampleWorld) where

import CommonData

sampleWorld :: World
sampleWorld = World sampleMap [] samplePlayer

sampleMap :: Map
sampleMap = _map

sampleUsername :: String
sampleUsername = "DanielDv99"

samplePlayer :: Entity
samplePlayer = Entity (Player playerData) (0, 0) (0, 0) (0, 0)
    where
        playerData = PlayerData [] 0 100 0 sampleUsername