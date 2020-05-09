module Rendering (renderWorld) where

import Graphics.Gloss
import Control.Lens

import CommonData
import Textures

-- TODO add translation to myPlayerPosition - screenWidth/2 (and same for height)
renderWorld :: World -> Picture
renderWorld world 
    =  renderMap (world ^. worldMap)
    <> renderEntities allEntities 
    <> renderUI (world ^. myPlayer . entityData)
    where
        allEntities = world ^. myPlayer : world ^. entities  

-- In case we want to see collision (for DEBUG purposes only)
-- we need to draw them additionaly here 
renderEntities :: [Entity] -> Picture
renderEntities entities = foldr (<>) Blank pictures  where
    pictures :: [Picture]
    pictures = map _entityTexture entities

renderMap :: Map -> Picture
renderMap m = foldr (<>) (_background m) blocks where
    blocks = map (\b ->
         uncurry translate (b ^. blockPosition) (b ^. blockTexture)) (_tiles m)

-- TODO
renderUI :: EntityData -> Picture
renderUI pd = color blue $ circleSolid 15