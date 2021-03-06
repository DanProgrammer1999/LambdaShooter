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

renderWorld :: Universe -> Picture
renderWorld (Universe world graphics) 
    =  renderMap graphics (world ^. worldMap)
    <> renderEntities graphics allEntities
    <> renderBodies (map _entityBody allEntities)
    <> renderUI world
    where
        allEntities = world ^. myPlayer : (world ^. players ++ world ^. projectiles)

-- | In case we want to see collision (for DEBUG purposes only)
-- | we need to draw them additionaly here 
renderEntities :: GameGraphics -> [Entity] -> Picture
renderEntities graphics entities = mconcat pictures  where
    pictures :: [Picture]
    pictures = map (entityToPicture graphics) entities

--   TODO make scale on load, not every frame.
-- | Render all Entities. Also translates and flip accordingly. 
entityToPicture :: GameGraphics -> Entity -> Picture
entityToPicture graphics entity = scaledAndTranslatedPic 
    where
        position = entity ^. entityBody . bodyPosition
        
        scaledPic = scale entitiesScale entitiesScale correctPic
        scaledAndTranslatedPic =
            uncurry translate position scaledPic

        correctPic 
            | isProjectile entity = graphics ^. bulletPicture 
            | isPlayer entity = getPlayerPicture graphics entity

getPlayerPicture :: GameGraphics -> Entity -> Picture
getPlayerPicture graphics player
    = healthBar <> namePicture <> uncurry translate textureShift playerPic
    where
        directionMultiplier =
            if player ^. direction == LeftDirection then -1 else 1
        textureShift = mulSV directionMultiplier (100, 0)
        eData = player ^. entityData
        animation = fromMaybe getDefaultAnimation (getAnimationFromEntity graphics eData)
        foundPic = case player ^. direction of
            RightDirection -> animation ^? frames . element (animation ^. curFrame)
            LeftDirection  -> animation ^? flippedFrames . element (animation ^. curFrame)

        playerPic = fromMaybe getDefaultPicture foundPic
        playerName = fromMaybe "" $ player ^? entityData . name
        playerLevel = fromMaybe 0 $ player ^? entityData . statistics . level

        namePicture
            = text (playerName ++ " (" ++ show playerLevel ++ " lvl.)")
            & color red 
            & translate (-playerPictureWidth/2 - 50) (playerPictureHeight/2 + nameYOffset)
            & scale 0.7 0.7
        
        playerHealth = fromMaybe 0 (player ^? entityData . health)
        healthBar 
            = renderHealthBar playerHealth
            & translate 0 (playerPictureHeight/2 + healthbarYOffset)

renderBodies ::  [Body] -> Picture
renderBodies bodies = mconcat pictures where
    pictures = map bodyToPicture bodies

bodyToPicture :: Body -> Picture
bodyToPicture body = uncurry translate (body ^. bodyPosition) pic where
    pic = renderCollisionBox (body ^. bodyCollisionBox)

renderCollisionBox :: CollisionBox -> Picture
renderCollisionBox (RectangleBox w h) = color getBodyColor $ rectangleWire w h
renderCollisionBox (CircleBox r) =  color getBodyColor $ circle r

renderMap :: GameGraphics -> Map -> Picture
renderMap graphics m = (graphics ^. backgroundPicture) <> mconcat blocksPics
    where
        blocksPics = map (renderBlock graphics) (m ^. blocks)

renderBlock :: GameGraphics -> Block -> Picture
renderBlock graphics b 
    = uncurry translate (b ^. blockPosition) scaledBlock where
        scaledBlock = scale xScale yScale (graphics ^. blockPicture)
        (textureWidth, textureHeight) = blockTextureSize
        width  = b ^. blockWidth
        height = b ^. blockHeight
        xScale = width  / textureWidth
        yScale = height / textureHeight 

renderHealthBar :: Health -> Picture
renderHealthBar playerHealth = healthContainer <> remainingBar
    where
        healthContainer = rectangleSolid healthBarLength 10
        healthPercent = playerHealth / maxHealth

        remainingLength = healthBarLength * healthPercent
        remainingBar
            = translate (-(healthBarLength - remainingLength)/2) 0
            $ color green
            $ rectangleSolid (healthBarLength*healthPercent) 10

-- Statistics
renderUI :: World -> Picture
renderUI world = deathsText <> killsText
    where
        playerStats = myPlayer . entityData . statistics
        playerKills = fromMaybe 0 $ world ^? playerStats . kills
        playerDeaths = fromMaybe 0 $ world ^? playerStats . deaths

        (width, height) = defaultWindowSize
        (width', height') = (fromIntegral width, fromIntegral height)

        killsText 
            = text ("Killed: " ++ show playerKills) 
            & color green
            & scale 0.15 0.15
            & translate (-50) (-height'/2 + 150)
            
        deathsText 
            = text ("Died " ++ show playerDeaths ++ " times") 
            & color red
            & scale 0.15 0.15
            & translate (-50) (-height'/2 + 120)
