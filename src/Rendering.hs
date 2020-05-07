module Rendering (renderWorld) where

import Graphics.Gloss
import CommonData

-- TODO add translation to myPlayerPosition - screenWidth/2 (and same for height)
renderWorld :: World -> Picture
renderWorld world = renderUI <> renderEntities (entities world) <> renderMap (map world) <> background

-- search how to load our pictures
background :: Picture

renderEntities :: [Entity] -> Picture

-- choose player picture (maybe some combination of geometric figures)
-- add random color to players
-- maybe outline our player (e.g. with red line)
renderPlayers :: [Player] -> Picture

renderMap :: Map -> Picture

renderUI :: Player -> Picture