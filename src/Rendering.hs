{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE TemplateHaskell #-}
module Rendering where

import           Graphics.Gloss
import           Graphics.Gloss.Rendering as RS
import           "GLFW-b" Graphics.UI.GLFW         as GLFW

import           Data.Maybe               (fromJust)
import           Game
import           Game.Player

import           Control.Lens
import           Graphics.Gloss.Juicy


newtype Textures = Textures {
                        _playert :: Picture
                        }deriving Show

makeLenses ''Textures



renderFrame :: Textures -> GameState -> Window -> RS.State -> IO ()
renderFrame textures gamestate window glossState = do
   displayPicture (width, height) white glossState 1.0 $ Pictures [rectangleSolid 640 480,
                                                                   translate (-320) (-240) (color red $ rectangleSolid 40 40),
                                                                   translate 320 (-240) (color red $ rectangleSolid 40 40),
                                                                   translate (-320) 240 (color red $ rectangleSolid 40 40),
                                                                   translate 320 240 (color red $ rectangleSolid 40 40),
                                                                   translate xpos ypos  (textures ^. playert),
                                                                   translate xpos ypos $ rectangleWire playerSize playerSize
                                                                  ]
   swapBuffers window

   where (xpos, ypos) = gamestate ^. player.position



loadTextures :: IO Textures
loadTextures = do
    texture    <- fromJust <$> loadJuicy "assets/alienBlue.png"
    return $ Textures texture
