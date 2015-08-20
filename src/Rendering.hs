{-# LANGUAGE PackageImports #-}
module Rendering (
    renderFrame,
    Resources,
    loadResources
    )
where

import           Graphics.Gloss
import           Graphics.Gloss.Data.Vector   (mulSV)
import           Graphics.Gloss.Data.ViewPort
import           Graphics.Gloss.Rendering     as RS
import           "GLFW-b" Graphics.UI.GLFW             as GLFW

import           Data.Maybe                   (fromJust)
import qualified Data.Vector                  as Vector
import           Game
import           Game.Blocks                  (Block, blockType)
import qualified Game.Blocks                  as Blocks
import           Game.Entities.Player         (Player)
import qualified Game.Entities.Player         as Pl
import           Game.Levels                  (background, levelBounds, tiles, enemies)

import           Control.Monad.Reader

import           Control.Lens
import           Graphics.Gloss.Juicy

import qualified Data.Map.Strict              as Map

import           Data.Monoid                  ((<>))
import qualified Game.Levels.Backgrounds             as B (Background (..),
                                                    BackgroundElement (..))

import           Game.Entities                (position, xy, velocity, dx)
import           Game.Entities.Fly            (Fly)

type Textures = Texture -> Picture
type Animations = Animation -> Vector.Vector Picture


data Texture = Background
             | AlienBlue
             | AlienBlueJump
             | Sandcenter
             | SandTop
             | Box
             | Cactus
             | Clouds1
             | Hills1
             | Hills2
             | Sun
             deriving (Ord, Eq, Show)

data Animation = AlienBlueWalk
               | Fly
               deriving (Ord, Eq, Show)


type Resources = (Textures, Animations)


renderFrame :: Resources ->  Window -> RS.State -> ReaderT GameState IO ()
renderFrame resources window glossState = do
    time      <- asks (^. totalTime)
    picPlayer <- renderPlayer resources time <$> asks (^. player)
    picBlocks <- foldMap (renderBlock resources) <$> asks (^. level.tiles)
    picEnemy <- foldMap (renderEnemy resources time) <$> asks (^. level.enemies)
    viewp     <- asks (^. viewport)
    (w, h) <- asks (^. level.levelBounds)

    background_ <- asks (^.level.background)

    lift $ displayPicture (width, height) (B._fillColor background_) glossState (viewPortScale viewp) $
                renderBackground resources background_ viewp -- Draw background
                <>
                applyViewPortToPicture viewp (rectangleWire  w h) -- Draw level boundaries
                <>
                applyViewPortToPicture viewp (picBlocks<>picEnemy<>picPlayer) -- Draw blocks and player
    lift $ swapBuffers window



renderPlayer :: Resources -> Double -> Player -> Picture
renderPlayer (textures, animations) time _player = translate xpos ypos (facing picture)
            where (xpos, ypos) = _player ^. position.xy
                  moving  = abs(_player ^. velocity.dx) > 1
                  onground = _player ^. Pl.onGround
                  picture
                      | moving && onground = animations AlienBlueWalk Vector.! mod (round (20*time) ) 9
                      | onground           = textures AlienBlue
                      | otherwise          = textures AlienBlueJump
                  facing pic = if (_player ^. velocity.dx) < -1 then scale (-1) 1 pic else pic


renderBlock :: Resources -> Block -> Picture
renderBlock (textures, _) block = case block ^. blockType of
                                        Blocks.SandTop    ->  renderWith $ textures SandTop
                                        Blocks.SandCenter ->  renderWith $ textures Sandcenter
                                        Blocks.Box        ->  renderWith $ textures Box
                                        Blocks.Cactus     ->  renderWith $ textures Cactus
                                  where renderWith = uncurry translate (block ^. position.xy)



renderBackground :: Resources -> B.Background -> ViewPort -> Picture
renderBackground res backg viewp = Pictures [ renderElement (B._statics backg),
                                              translate o1x o1y (renderElement (B._parallax1 backg)),
                                              translate o2x o2y (renderElement (B._parallax2 backg)),
                                              translate o3x o3y (renderElement (B._parallax3 backg))
                                            ]

                                 where textures = fst res
                                       (o1x, o1y) = mulSV 0.05 (viewPortTranslate viewp)
                                       (o2x, o2y) = mulSV 0.1 (viewPortTranslate viewp)
                                       (o3x, o3y) = mulSV 0.2 (viewPortTranslate viewp)
                                       renderElement B.Sun = translate 200 150 (textures Sun)
                                       renderElement B.Clouds1 = translate 0 (-70) (textures Clouds1)
                                       renderElement B.Hills2 = translate 0 (-120) (textures Hills2)
                                       renderElement B.Hills1 = translate 0 (-150) (textures Hills1)
                                       renderElement B.Empty = Blank


renderEnemy :: Resources -> Double -> Fly -> Picture
renderEnemy (_, animations) time fly = translate xpos ypos (facing picture)
            where (xpos, ypos) = fly ^. position.xy
                  picture = animations Fly Vector.! mod (round (20*time) ) 2
                  facing pic = if (fly ^. velocity.dx) > 1 then scale (-1) 1 pic else pic



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
                                                                       (Cactus, "assets/cactus.png"),
                                                                       (Sun, "assets/backgrounds/sun.png"),
                                                                       (Clouds1, "assets/backgrounds/clouds1.png"),
                                                                       (Hills1, "assets/backgrounds/hills1.png"),
                                                                       (Hills2, "assets/backgrounds/hills2.png")
                                                                      ]))
        where fun (texture, pic) = do
                                     p <- loadPng pic
                                     return (texture, p)

loadAnimations :: IO Animations
loadAnimations = liftM (fromJust.) (flip Map.lookup <$> (Map.fromList <$> mapM fun [
                                (AlienBlueWalk, Vector.fromList ["assets/alienBlueWalk/p2_walk0"++ show (n::Int) ++ ".png" | n <- [1..9]]),
                                (Fly, Vector.fromList ["assets/fly/fly"++ show (n::Int) ++ ".png" | n <- [1..2]])
                              ]))
    where fun (texture, ls) = do
                       pics <- mapM loadPng ls
                       return (texture, pics)

loadResources :: IO Resources
loadResources = do
    textures <- loadTextures
    animations <- loadAnimations
    return (textures, animations)



