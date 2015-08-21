module Resources where

import           Graphics.Gloss

import           Data.Maybe                   (fromJust)
import qualified Data.Vector                  as Vector

import Control.Monad(liftM)

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



