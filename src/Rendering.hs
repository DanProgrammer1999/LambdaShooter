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
renderEntities entities = mconcat pictures 
    where
        pictures = map (view entityTexture) entities

renderMap :: Map -> Picture
renderMap m = (m ^. background) <> mconcat blocksPics 
    where
        blocksPics = map renderBlock (m ^. blocks)

renderBlock :: Block -> Picture
renderBlock b = uncurry translate (b ^. blockPosition) (b ^. blockTexture)

-- TODO
renderUI :: EntityData -> Picture
renderUI pd = color blue $ circleSolid 15