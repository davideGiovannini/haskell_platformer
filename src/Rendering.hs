{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE TemplateHaskell #-}
module Rendering (
    renderFrame,
    Textures,
    loadTextures
    )
where

import           Graphics.Gloss
import           Graphics.Gloss.Rendering as RS
import           "GLFW-b" Graphics.UI.GLFW         as GLFW

import           Data.Maybe               (fromJust)
import           Game
import qualified Game.Blocks              as Blocks
import           Game.Player

import           Control.Lens
import           Graphics.Gloss.Juicy


data Textures = Textures {
                        _backgroundT :: Picture,
                        _playerT     :: Picture,
                        _sandcenterT :: Picture,
                        _sandTopT    :: Picture,
                        _boxT        :: Picture

                        }deriving Show

makeLenses ''Textures



renderFrame :: Textures -> GameState -> Window -> RS.State -> IO ()
renderFrame textures gamestate window glossState = do
   displayPicture (width, height) black glossState 1.0 $ Pictures ( [textures ^. backgroundT
                                                                  ] ++ picBlocks ++ picPlayer )
   swapBuffers window

   where (xpos, ypos) = gamestate ^. player.position
         picPlayer = [translate xpos ypos  (textures ^. playerT), translate xpos ypos $ rectangleWire playerSize playerSize]
         picBlocks = map (renderBlock textures) (gamestate ^. blocks)



renderBlock :: Textures -> Blocks.Block -> Picture
renderBlock textures (Blocks.SandTop (x,y)) =  translate x y (textures ^.sandTopT)
renderBlock textures (Blocks.SandCenter (x,y)) =  translate x y (textures ^.sandcenterT)
renderBlock textures (Blocks.Box (x,y)) =  translate x y (textures ^.boxT)


loadTextures :: IO Textures
loadTextures = do
    background_ <- fromJust <$> loadJuicy "assets/uncolored_peaks.png"
    player_ <- fromJust <$> loadJuicy "assets/alienBlue.png"
    sand_   <- fromJust <$> loadJuicy "assets/sandCenter.png"
    sandtop_<- fromJust <$> loadJuicy "assets/sandMid.png"
    box_    <- fromJust <$> loadJuicy "assets/box.png"

    return $ Textures background_ player_ sand_ sandtop_ box_
