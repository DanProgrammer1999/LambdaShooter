module Rendering (renderWorld) where

import Graphics.Gloss
import Control.Lens

import CommonData
import Animation
import Constants
import Prelude hiding (flip)

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
renderEntities entities = mconcat pictures  where
    pictures :: [Picture]
    pictures = map (scale entitiesScale entitiesScale . entityToPicture) entities


-- ^ Render all Entities and translates and flip accordingly. 
entityToPicture :: Entity -> Picture
entityToPicture entity = pic3 where
    pic1 = if isPlayer entity
        then playerPic
        else entity ^. entityTexture

    pic2 = if (entity ^. direction) == LeftDirection
        then flip pic1 
        else pic1

    pic3 = uncurry translate (entity ^. entityBody . bodyPosition) pic2

    animation = getAnimationFromEntity entity
    playerPic = case animation of
        Just a -> 
            let foundPic = (a ^. frames) ^? element (a ^. curFrame) in
                case foundPic of
                    Just p -> p
                    Nothing ->  error $ "No such animation sprite with index "
                     ++ show (a ^. curFrame) 
        Nothing -> error $ "No such animation with state " ++
         show (_currentState (entity ^. entityData))

renderMap :: Map -> Picture
renderMap m = (m ^. background) <> mconcat blocksPics 
    where
        blocksPics = map renderBlock (m ^. blocks)

renderBlock :: Block -> Picture
renderBlock b = uncurry translate (b ^. blockPosition) (b ^. blockTexture)

-- TODO
renderUI :: EntityData -> Picture
renderUI pd = color blue $ circleSolid 15

-- ^ Flips picture. 
flip :: Picture -> Picture
flip  = id