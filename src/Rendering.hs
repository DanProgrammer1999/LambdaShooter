module Rendering (renderWorld) where

import Graphics.Gloss
import Control.Lens

import CommonData
import Textures

-- TODO add translation to myPlayerPosition - screenWidth/2 (and same for height)
renderWorld :: World -> Picture
renderWorld world 
    =  renderMap (world ^. worldMap)
    <> renderEntities (world ^. entities)
    <> renderPlayers (world ^. myPlayer)
    <> renderUI (world ^. myPlayer . entityData)  

-- In case we want to see collision (for DEBUG purposes only)
-- we need to draw them additionaly here 
renderEntities :: [Entity] -> Picture
renderEntities entities = mconcat pictures 
    where
        pictures = map (view entityTexture) entities

renderPlayers :: [Entity] -> Picture
renderPlayer players = pic where
    pic = 

renderMap :: Map -> Picture
renderMap m = (m ^. background) <> mconcat blocks 
    where
        blocks = map renderBlock (m ^. tiles)

renderBlock :: Block -> Picture
renderBlock b = uncurry translate (b ^. blockPosition) (b ^. blockTexture)

-- TODO
renderUI :: EntityData -> Picture
renderUI pd = color blue $ circleSolid 15