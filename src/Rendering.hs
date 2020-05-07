module Rendering where

import Graphics.Gloss
import CommonData

renderWorld :: World -> Picture
renderWorld world = renderUI <> renderEntities (entities world) <> renderMap (map world) <> background

background :: Picture

renderEntities :: [WorldEntity] -> Picture

renderMap :: Map -> Picture

renderUI :: Player -> Picture