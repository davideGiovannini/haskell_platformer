{-# LANGUAGE PackageImports #-}
module Rendering (
    renderFrame,
    Resources,
    loadResources
    )
where

import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Graphics.Gloss.Rendering     as RS
import           "GLFW-b" Graphics.UI.GLFW             as GLFW

import           Data.Maybe                   (fromJust)
import qualified Data.Vector                  as Vector
import           Game
import qualified Game.Blocks                  as Blocks
import qualified Game.Player                  as Pl

import           Control.Monad.Reader

import           Control.Lens
import           Graphics.Gloss.Juicy

import qualified Data.Map.Strict              as Map

type Textures = Texture -> Picture
type Animations = Animation -> Vector.Vector Picture


data Texture = Background
             | AlienBlue
             | AlienBlueJump
             | Sandcenter
             | SandTop
             | Box
             | Sun
             deriving (Ord, Eq, Show)

data Animation = AlienBlueWalk
               deriving (Ord, Eq, Show)


type Resources = (Textures, Animations)


renderFrame :: Resources ->  Window -> RS.State -> ReaderT GameState IO ()
renderFrame resources@(textures, _) window glossState = do
    time      <- asks (^. totalTime)
    picPlayer <- renderPlayer resources time <$> asks (^. player)
    picBlocks <- map (renderBlock resources) <$> asks (^.  blocks)
    viewp     <- asks (^. viewport)

    lift $ displayPicture (width, height) black glossState (viewPortScale viewp) $
        Pictures (textures Background:translate 200 150 (textures Sun) :[applyViewPortToPicture viewp(Pictures $ picBlocks ++ [picPlayer])])
    lift $ swapBuffers window




renderPlayer :: Resources -> Double -> Pl.Player -> Picture
renderPlayer (textures, animations) time _player = translate xpos ypos (facing picture)
            where (xpos, ypos) = _player ^. Pl.position
                  moving  = abs(_player ^. Pl.velocity._1) > 1
                  onground = _player ^. Pl.onGround
                  picture
                      | moving && onground = animations AlienBlueWalk Vector.! mod (round (20*time) ) 9
                      | onground           = textures AlienBlue
                      | otherwise          = textures AlienBlueJump
                  facing pic = if (_player ^. Pl.velocity._1) < -1 then scale (-1) 1 pic else pic


renderBlock :: Resources -> Blocks.Block -> Picture
renderBlock (textures, _) (Blocks.SandTop (x,y))    =  translate x y (textures SandTop)
renderBlock (textures, _) (Blocks.SandCenter (x,y)) =  translate x y (textures Sandcenter)
renderBlock (textures, _) (Blocks.Box (x,y))        =  translate x y (textures Box)


loadPng :: String -> IO Picture
loadPng path = fromJust <$> loadJuicy path

loadTextures :: IO Textures
loadTextures =  liftM (fromJust.)
                      (flip Map.lookup <$> (Map.fromList <$> mapM fun [(Background, "assets/uncolored_peaks.png"),
                                                                       (AlienBlue, "assets/alienBlue.png"),
                                                                       (AlienBlueJump, "assets/alienBlue_jump.png"),
                                                                       (Sandcenter, "assets/sandCenter.png"),
                                                                       (SandTop, "assets/sandMid.png"),
                                                                       (Box, "assets/box.png"),
                                                                       (Sun, "assets/sun.png")
                                                                      ]))
        where fun (texture, pic) = do
                                     p <- loadPng pic
                                     return (texture, p)

loadAnimations :: IO Animations
loadAnimations = liftM (fromJust.) (flip Map.lookup <$> (Map.fromList <$> mapM fun [
                                (AlienBlueWalk, Vector.fromList ["assets/alienBlueWalk/p2_walk0"++ show (n::Int) ++ ".png" | n <- [1..9]])
                              ]))
    where fun (texture, ls) = do
                       pics <- mapM loadPng ls
                       return (texture, pics)

loadResources :: IO Resources
loadResources = do
    textures <- loadTextures
    animations <- loadAnimations
    return (textures, animations)



