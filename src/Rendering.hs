module Rendering (renderWorld) where

import Control.Lens
import Prelude hiding (flip)

import Graphics.Gloss
import Graphics.Gloss.Data.Vector

import CommonData
import Animation
import Constants
import Constructors (getDefaultPicture)
import Data.Maybe

renderWorld :: GameGraphics -> World -> Picture
renderWorld gameGraphics world
    =  renderMap (world ^. worldMap)
    <> renderEntities allEntities
    <> renderBodies (map _entityBody allEntities)
    <> renderUI (world ^. myPlayer . entityData)
    where
        allEntities = world ^. myPlayer : (world ^. players ++ world ^. projectiles)

-- | In case we want to see collision (for DEBUG purposes only)
-- | we need to draw them additionaly here 
renderEntities :: [Entity] -> Picture
renderEntities entities = mconcat pictures  where
    pictures :: [Picture]
    pictures = map entityToPicture entities


-- | Render all Entities. Also translates and flip accordingly. 
entityToPicture :: Entity -> Picture
entityToPicture entity = scaledAndTranslatedPic where
    position = entity ^. entityBody . bodyPosition

    rightPic =
        if   isPlayer entity
        then playerPic
        else entity ^. entityTexture
    scaledPic = scale entitiesScale entitiesScale rightPic
    scaledAndTranslatedPic =
         uncurry translate position scaledPic

    playerPic = getPlayerPicture entity

getPlayerPicture :: Entity -> Picture
getPlayerPicture player
    = playerName <> uncurry translate textureShift playerPic
    where
        directionMultiplier =
            if player ^. direction == LeftDirection then -1 else 1
        textureShift = mulSV directionMultiplier (100, 0)

        animation = fromMaybe getDefaultAnimation (getAnimationFromEntity player)
        foundPic = case player ^. direction of
            RightDirection -> animation ^? frames . element (animation ^. curFrame)
            LeftDirection  -> animation ^? flippedFrames . element (animation ^. curFrame)
        playerPic = fromMaybe getDefaultPicture foundPic

        playerName = color red $ uncurry translate namePosition scaled
            where
                namePosition = (-75, 250)
                scaled = scale 0.75 0.75 namePicture
                namePicture = text (fromMaybe "" (player ^? entityData . name))

renderBodies ::  [Body] -> Picture
renderBodies bodies = mconcat pictures where
    pictures = map bodyToPicture bodies

bodyToPicture :: Body -> Picture
bodyToPicture body = uncurry translate (body ^. bodyPosition) pic where
    pic = renderCollisionBox (body ^. bodyCollisionBox)

renderCollisionBox :: CollisionBox -> Picture
renderCollisionBox (RectangleBox w h) = color getBodyColor $ rectangleWire w h
renderCollisionBox (CircleBox r) =  color getBodyColor $ circleSolid r

renderMap :: Map -> Picture
renderMap m = (m ^. background) <> mconcat blocksPics
    where
        blocksPics = map renderBlock (m ^. blocks)

renderBlock :: Block -> Picture
renderBlock b = uncurry translate (b ^. blockPosition) (b ^. blockTexture)

renderUI :: EntityData -> Picture
renderUI playerData = translate 0 350 $ healthContainer <> remainingBar
    where
        healthBarLength = 800.0
        healthContainer = rectangleSolid healthBarLength 10
        healthPercent = fromMaybe 0 (playerData ^? health) / maxHealth

        remainingLength = healthBarLength * healthPercent
        remainingBar
            = translate (-(healthBarLength - remainingLength)/2) 0
            $ color green
            $ rectangleSolid (healthBarLength*healthPercent) 10
