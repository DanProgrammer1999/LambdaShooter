module Rendering (renderWorld) where

import Graphics.Gloss
import Control.Lens

import CommonData
import Textures

-- TODO add translation to myPlayerPosition - screenWidth/2 (and same for height)
renderWorld :: World -> Picture
renderWorld world 
    =  renderUI (world ^. myPlayer . playerData)
    <> renderBodies allBodies 
    <> renderMap (world ^. worldMap)  
    <> background
    where
        allBodies = world ^. entities ++ allPlayers
        allPlayers = (world ^. myPlayer . playerBody) : (map (view playerBody) (world ^. players))

renderBodies :: [Body] -> Picture
renderBodies entities = foldr (<>) Blank pictures  where
    pictures :: [Picture]
    pictures = map texture entities

renderMap :: Map -> Picture
renderMap m = color black $ circleSolid 110

renderUI :: PlayerData -> Picture
renderUI pd = color blue $ circleSolid 15