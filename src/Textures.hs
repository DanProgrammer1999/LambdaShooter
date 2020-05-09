{-# LANGUAGE OverloadedStrings #-}

module Textures where

import Graphics.Gloss
import Graphics.Gloss.Juicy 

-- Texture is the same as sprite in this project.
type TextureName = String

textureNames :: [TextureName]
textureNames = [backgroundName, playerName]

backgroundName :: TextureName
backgroundName = "resources/background.png"

backgroundTexture :: [(TextureName, Picture)] -> Maybe Picture
backgroundTexture = lookup backgroundName

playerName :: TextureName
playerName = "resources/terrorist_1/idle/1_terrorist_1_Idle_000.png"

playerTexture :: [(TextureName, Picture)] -> Maybe Picture
playerTexture = lookup playerName

loadTexture :: TextureName -> IO Picture 
loadTexture textureName = do 
    pic <- loadJuicyPNG textureName
    case pic of
        Just pic -> return pic
        Nothing -> do
            -- maybe we need to stop execution if smth goes wrong
             let errorMsg = "Texture \"" ++ textureName ++ "\" was not found."
             print errorMsg
             return blank