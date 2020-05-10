module Rendering (renderWorld) where

import Graphics.Gloss
import Control.Lens

import CommonData
import Animation
import Prelude hiding (Left, Right)

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
    pictures = map entityToPicture entities


-- ^ Render all Entities and translates and flip accordingly. 
entityToPicture :: Entity -> Picture
entityToPicture entity = rightPic where
    rightPic = if isPlayer entity
        then playerPic
        else entity ^. entityTexture
    -- ^ Flip if Direction == Left.
    -- pic = if (entity ^. entityData . direction) == Left
    --     then flip rightPic
    --     else rightPic
    -- ^ Change flip to REALLY flip.
    flip = id
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