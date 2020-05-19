module Rendering (renderWorld) where

import Graphics.Gloss
import Control.Lens

import CommonData
import Animation
import Constants
import Prelude hiding (flip)

renderWorld :: World -> Picture
renderWorld world 
    =  renderMap (world ^. worldMap)
    <> renderEntities allEntities
    <> renderBodies (map _entityBody allEntities) 
    <> renderUI (world ^. myPlayer . entityData)
    where
        allEntities = world ^. myPlayer : world ^. entities  

-- | In case we want to see collision (for DEBUG purposes only)
-- | we need to draw them additionaly here 
renderEntities :: [Entity] -> Picture
renderEntities entities = mconcat pictures  where
    pictures :: [Picture]
    pictures = map (scale entitiesScale entitiesScale . entityToPicture) entities


-- | Render all Entities and translates and flip accordingly. 
-- TODO TOFIX Alex: fix scaling issues
-- TODO TOFIX Alex: fix error on state change
entityToPicture :: Entity -> Picture
entityToPicture entity = pic where
    pic1 = if isPlayer entity
        then playerPic
        else entity ^. entityTexture
    pic = uncurry translate (entity ^. entityBody . bodyPosition) pic1
    animation = getAnimationFromEntity entity
    playerPic = case animation of
        Just a -> 
            let foundPic = (if (entity ^. direction) == RightDirection
                then a ^? frames . element (a ^. curFrame)
                else a ^? flippedFrames . element (a ^. curFrame)) in 
                case foundPic of
                    Just p -> p
                    Nothing ->  error $ "No such animation sprite with index "
                     ++ show (a ^. curFrame) 
        Nothing -> error $ "No such animation with state " ++
         show (_currentState (entity ^. entityData)) 
         ++ "; All states are: " ++ show (entity ^. entityData . animations)

renderBodies ::  [Body] -> Picture
renderBodies bodies = mconcat pictures where
    pictures = map bodyToPicture bodies

bodyToPicture :: Body -> Picture
bodyToPicture body = uncurry translate (body ^. bodyPosition) pic where
    pic = renderCollisionBox (body ^. bodyCollisionBox)

renderCollisionBox :: CollisionBox -> Picture
renderCollisionBox (RectangleBox w h) = color getBodyColor $ rectangleSolid w h
renderCollisionBox (CircleBox r) =  color getBodyColor $ circleSolid r

renderMap :: Map -> Picture
renderMap m = (m ^. background) <> mconcat blocksPics 
    where
        blocksPics = map renderBlock (m ^. blocks)

renderBlock :: Block -> Picture
renderBlock b = uncurry translate (b ^. blockPosition) (b ^. blockTexture)

-- TODO
renderUI :: EntityData -> Picture
renderUI pd = color yellow $ circleSolid 15
