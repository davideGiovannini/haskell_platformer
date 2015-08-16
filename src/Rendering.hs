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
import qualified Game.Player as Pl

import           Control.Lens
import           Graphics.Gloss.Juicy


data Textures = Textures {
                        _backgroundT     :: Picture,
                        _alienBlueT      :: Picture,
                        _alienBlueJumpT  :: Picture,
                        _alienBlueStandT :: Picture,
                        _sandcenterT     :: Picture,
                        _sandTopT        :: Picture,
                        _boxT            :: Picture

                        }deriving Show

makeLenses ''Textures



renderFrame :: Textures -> GameState -> Window -> RS.State -> IO ()
renderFrame textures gamestate window glossState = do
   displayPicture (width, height) black glossState 1.0 $ Pictures ( [textures ^. backgroundT
                                                                  ] ++ picBlocks ++ picPlayer )
   swapBuffers window

   where
         picPlayer = [renderPlayer textures (gamestate ^. player )]
         picBlocks = map (renderBlock textures) (gamestate ^. blocks)


renderPlayer :: Textures -> Pl.Player -> Picture
renderPlayer textures _player = translate xpos ypos (facing (textures ^. picture))
            where (xpos, ypos) = _player ^. Pl.position
                  moving  = abs(_player ^. Pl.velocity._1) > 1
                  onground = _player ^. Pl.onGround
                  picture
                      | moving && onground = alienBlueStandT
                      | onground           = alienBlueT
                      | otherwise          = alienBlueJumpT
                  facing pic = if (_player ^. Pl.velocity._1) < -1 then scale (-1) 1 pic else pic


renderBlock :: Textures -> Blocks.Block -> Picture
renderBlock textures (Blocks.SandTop (x,y)) =  translate x y (textures ^.sandTopT)
renderBlock textures (Blocks.SandCenter (x,y)) =  translate x y (textures ^.sandcenterT)
renderBlock textures (Blocks.Box (x,y)) =  translate x y (textures ^.boxT)


loadTextures :: IO Textures
loadTextures =
    Textures <$> load "assets/uncolored_peaks.png"
             <*> load "assets/alienBlue.png"
             <*> load "assets/alienBlue_jump.png"
             <*> load "assets/alienBlue_stand.png"
             <*> load "assets/sandCenter.png"
             <*> load "assets/sandMid.png"
             <*> load "assets/box.png"
    where load path = fromJust <$> loadJuicy path

