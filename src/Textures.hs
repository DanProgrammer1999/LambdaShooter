{-# LANGUAGE OverloadedStrings #-}

module Textures where

import Graphics.Gloss
import Graphics.Gloss.Juicy 

-- Texture is the same as sprite in this project.
type TextureName = String

textureNames :: [TextureName]
textureNames = [backgroundName, foregroundName, heroName]

backgroundName :: TextureName
backgroundName = "game.png"

backgroundTexture :: [(TextureName, Picture)] -> Maybe Picture
backgroundTexture = lookup backgroundName

foregroundName :: TextureName
foregroundName = "particles.png"

foregroundTexture :: [(TextureName, Picture)] -> Maybe Picture
foregroundTexture = lookup foregroundName

heroName :: TextureName
heroName = "hero.png"

heroTexture :: [(TextureName, Picture)] -> Maybe Picture
heroTexture = lookup heroName


loadTexture :: TextureName -> IO Picture 
loadTexture textureName = do 
    pic <- loadJuicyPNG ("resources/" ++ textureName)
    case pic of
        Just pic -> return pic
        Nothing -> do
             let errorMsg = "Texture \"" ++ textureName ++ "\" was not found."
             print errorMsg
             return blank