module Rendering (renderWorld) where

import Graphics.Gloss
import CommonData
import Textures

-- TODO add translation to myPlayerPosition - screenWidth/2 (and same for height)
renderWorld :: World -> Picture
renderWorld world = 
 renderEntities (entities world) <> 
 renderMap (worldMap world) <> (background . worldMap world) where
     allEntities = entities world ++ Entity (Player . myPlayer world) (EntityData (0,0) (0,0) (0,0))

renderEntities :: [Entity] -> Picture
renderEntities entities = foldr (<>) Blank pictures  where
    pictures :: [Picture]
    pictures = map texture entities

renderMap :: Map -> Picture
renderMap m = color black $ circleSolid 110

renderUI :: PlayerData -> Picture
renderUI pd = color blue $ circleSolid 15