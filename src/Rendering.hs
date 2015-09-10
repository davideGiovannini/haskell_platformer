{-# LANGUAGE PackageImports #-}
module Rendering (
    renderFrame
    )
where

import           Data.Monoid                  ((<>))
import qualified Game.Levels.Backgrounds      as B

import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort (ViewPort, applyViewPortToPicture,
                                               viewPortScale, viewPortTranslate)

import           Graphics.Gloss.Data.Vector   (mulSV)


import           Graphics.Gloss.Rendering     as RS
import           "GLFW-b" Graphics.UI.GLFW             as GLFW

import qualified Data.Vector                  as Vector
import           Game                         (GameState (), background, height,
                                               totalTime, viewPort, width,
                                               world)

import           Control.Monad.Reader

import           Control.Lens                 hiding (from, has)


import           Components.Bounds            (wh)
import           Components.Direction
import           Components.Position
import           Components.Renderable
import           Components.Velocity
import           Entities
import           Systems


import           Resources


renderFrame :: Resources ->  Window -> RS.State -> ReaderT GameState IO ()
renderFrame resources window glossState = do
    time      <- asks (^. totalTime)
    setEntities <- asks (^. world.entities)
    viewP  <- asks (^. viewPort)
    backg  <- asks (^. background)
    (w, h) <- asks (^. world.dimensions.wh)

    let pics :: Picture
        pics  = foldMap (renderEntity $ renderElem resources time) setEntities


    lift $ displayPicture (width, height) (B._fillColor backg) glossState (viewPortScale viewP) $
                                    renderBackground resources backg viewP -- Draw background
                                    <>
                                    applyViewPortToPicture viewP (rectangleWire  w h) -- Draw level boundaries
                                    <>
                                    applyViewPortToPicture viewP pics

    lift $ swapBuffers window



renderEntity :: (Position -> Renderable -> Bool -> Picture) ->  Entity -> Picture
renderEntity renderFunc entity =
          if entity `has` renderable && entity `has` position then
                renderFunc (position `from` entity) (entity & renderable `from` entity) mustFlip
          else
            Blank
          where mustFlip = (entity `has` direction && entity `has` velocity)
                           &&
                           (let (Velocity dx' _) = velocity `from` entity
                            in
                            case direction `from` entity of
                                SpeedDirection          -> dx' < -1
                                InvertedSpeedDirection  -> dx' > 1
                           )


renderElem :: Resources -> Double -> Position -> Renderable -> Bool -> Picture
renderElem (textures, animations) time pos resource mustFlip =
    let pic = case resource of
                    RenderTexture texture  -> textures texture
                    RenderAnim frames anim -> animations anim Vector.! mod (round (20*time))frames
        pict = if mustFlip then scale (-1) 1 pic else pic
    in
    uncurry translate (pos ^. xy) pict


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



