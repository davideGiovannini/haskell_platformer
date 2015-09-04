{-# LANGUAGE PackageImports #-}
module Rendering (
    renderFrame
    )
where

import           Graphics.Gloss
import           Graphics.Gloss.Rendering as RS
import           "GLFW-b" Graphics.UI.GLFW         as GLFW

import qualified Data.Vector              as Vector
import           Game                     (GameState (), height, totalTime,
                                           width, world)

import           Control.Monad.Reader

import           Control.Lens hiding (has, from)


import           Components.Position
import           Components.Renderable
import           Entities
import           Systems


import           Resources


renderFrame :: Resources ->  Window -> RS.State -> ReaderT GameState IO ()
renderFrame resources window glossState = do
    time      <- asks (^. totalTime)
    setEntities <- asks (^. world.entities)
    let pics :: Picture
        pics  = foldMap (renderEntity $ renderElem resources time) setEntities



    {-picBlocks <- foldMap (renderBlock resources) <$> asks (^. level.tiles)-}
    {-picEnemy <- foldMap (renderEnemy resources time) <$> asks (^. level.enemies)-}
    {-viewp     <- asks (^. viewport)-}
    {-(w, h) <- asks (^. level.levelBounds)-}

    {-background_ <- asks (^.level.background)-}

    lift $ displayPicture (width, height) black glossState 0 pics

    {-lift $ displayPicture (width, height) (B._fillColor background_) glossState (viewPortScale viewp) $-}
                {-renderBackground resources background_ viewp -- Draw background-}
                {-<>-}
                {-applyViewPortToPicture viewp (rectangleWire  w h) -- Draw level boundaries-}
                {-<>-}
                {-applyViewPortToPicture viewp (picBlocks<>picEnemy<>picPlayer) -- Draw blocks and player-}



    lift $ swapBuffers window



renderEntity :: (Position -> Renderable -> Picture) -> Entity -> Picture
renderEntity renderFunc entity =
          if entity `has` renderable && entity `has` position then
                renderFunc (position `from` entity) (renderable `from` entity)
          else
            Blank

renderElem :: Resources -> Double -> Position -> Renderable -> Picture
renderElem (textures, _) _ pos (RenderTexture texture) = uncurry translate (pos ^.xy) (textures texture)
renderElem (_, animations) time pos (RenderAnim frames anim) = uncurry translate (pos ^. xy) (animations anim Vector.! mod (round (20*time))frames)

{-renderBackground :: Resources -> B.Background -> ViewPort -> Picture-}
{-renderBackground res backg viewp = Pictures [ renderElement (B._statics backg),-}
                                              {-translate o1x o1y (renderElement (B._parallax1 backg)),-}
                                              {-translate o2x o2y (renderElement (B._parallax2 backg)),-}
                                              {-translate o3x o3y (renderElement (B._parallax3 backg))-}
                                            {-]-}

                                 {-where textures = fst res-}
                                       {-(o1x, o1y) = mulSV 0.05 (viewPortTranslate viewp)-}
                                       {-(o2x, o2y) = mulSV 0.1 (viewPortTranslate viewp)-}
                                       {-(o3x, o3y) = mulSV 0.2 (viewPortTranslate viewp)-}
                                       {-renderElement B.Sun = translate 200 150 (textures Sun)-}
                                       {-renderElement B.Clouds1 = translate 0 (-70) (textures Clouds1)-}
                                       {-renderElement B.Hills2 = translate 0 (-120) (textures Hills2)-}
                                       {-renderElement B.Hills1 = translate 0 (-150) (textures Hills1)-}
                                       {-renderElement B.Empty = Blank-}



