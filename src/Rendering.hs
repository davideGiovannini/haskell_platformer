{-# LANGUAGE PackageImports  #-}
module Rendering (
    renderFrame,
    Resources,
    loadResources
    )
where

import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Graphics.Gloss.Rendering as RS
import           "GLFW-b" Graphics.UI.GLFW         as GLFW

import           Data.Maybe               (fromJust)
import           Game
import qualified Game.Blocks              as Blocks
import qualified Game.Player as Pl
import qualified Data.Vector as Vector

import Control.Monad.Reader

import           Control.Lens
import           Graphics.Gloss.Juicy

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

type Textures = Map Texture Picture
type Animations = Map Animation (Vector.Vector Picture)

tlookup :: Texture -> Textures -> Picture
tlookup texture textures = fromJust $ Map.lookup texture textures

alookup :: Animation -> Animations -> Vector.Vector Picture
alookup anim anims = fromJust $ Map.lookup anim anims

data Texture = Background
             | AlienBlue
             | AlienBlueJump
             | Sandcenter
             | SandTop
             | Box
             deriving (Ord, Eq, Show)

data Animation = AlienBlueWalk
               deriving (Ord, Eq, Show)


data Resources = Resources {
                            _animations :: Animations,
                            _textures   :: Textures
                        }


renderFrame :: Resources ->  Window -> RS.State -> ReaderT GameState IO ()
renderFrame resources window glossState = do
    time      <- asks (^. totalTime)
    picPlayer <- renderPlayer resources time <$> asks (^. player)
    picBlocks <- map (renderBlock resources) <$> asks (^.  blocks)
    viewp     <- asks (^. viewport)

    lift $ displayPicture (width, height) black glossState (viewPortScale viewp) $
        Pictures (tlookup Background (_textures resources) :[applyViewPortToPicture viewp(Pictures $ picBlocks ++ [picPlayer])])
    lift $ swapBuffers window




renderPlayer :: Resources -> Double -> Pl.Player -> Picture
renderPlayer resources time _player = translate xpos ypos (facing picture)
            where (xpos, ypos) = _player ^. Pl.position
                  moving  = abs(_player ^. Pl.velocity._1) > 1
                  onground = _player ^. Pl.onGround
                  picture
                      | moving && onground = alookup AlienBlueWalk (_animations resources) Vector.! mod (round (20*time) ) 9
                      | onground           = tlookup AlienBlue (_textures resources)
                      | otherwise          = tlookup AlienBlueJump (_textures resources)
                  facing pic = if (_player ^. Pl.velocity._1) < -1 then scale (-1) 1 pic else pic


renderBlock :: Resources -> Blocks.Block -> Picture
renderBlock resources (Blocks.SandTop (x,y))    =  translate x y (tlookup SandTop (_textures resources))
renderBlock resources (Blocks.SandCenter (x,y)) =  translate x y (tlookup Sandcenter (_textures resources))
renderBlock resources (Blocks.Box (x,y))        =  translate x y (tlookup Box (_textures resources))


loadPng :: String -> IO Picture
loadPng path = fromJust <$> loadJuicy path

loadTextures :: IO Textures
loadTextures =
    Map.fromList <$> mapM fun [(Background, "assets/uncolored_peaks.png"),
                               (AlienBlue, "assets/alienBlue.png"),
                               (AlienBlueJump, "assets/alienBlue_jump.png"),
                               (Sandcenter, "assets/sandCenter.png"),
                               (SandTop, "assets/sandMid.png"),
                               (Box, "assets/box.png")
                              ]
    where
          fun (texture, pic) = do
                             p <- loadPng pic
                             return (texture, p)

loadAnimations :: IO Animations
loadAnimations =
    Map.fromList <$> mapM fun [
                                (AlienBlueWalk, Vector.fromList ["assets/alienBlueWalk/p2_walk0"++ show (n::Int) ++ ".png" | n <- [1..9]])
                              ]
    where fun (texture, ls) = do
                       pics <- mapM loadPng ls
                       return (texture, pics)

loadResources :: IO Resources
loadResources = do
    textures <- loadTextures
    animations <- loadAnimations
    return $ Resources animations textures



