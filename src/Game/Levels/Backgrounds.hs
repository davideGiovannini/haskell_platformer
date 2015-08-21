module Game.Levels.Backgrounds where

{-import Graphics.Gloss (Color, makeColorI)-}

import Resources
import Components.Renderable
import Components.Position
import Entities

import Control.Monad.State.Strict

--data BackgroundElement = Sun
--                       | Clouds1
 --                      | Hills1
  --                     | Hills2
   --                    | Empty
    --                   deriving (Show, Eq)

--data Background = Background{
--                             _fillColor :: Color,
--                             _statics   :: BackgroundElement,
--                             _parallax1 :: BackgroundElement,
--                             _parallax2 :: BackgroundElement,
--                             _parallax3 :: BackgroundElement
--                            } deriving Show


{-desertBackground :: Background-}
{-desertBackground = Background (makeColorI 213 237 247 255) Sun Clouds1 Hills2 Hills1-}


desertBackground :: State World ()
desertBackground = do
    entity <- newEntity
    updateRenderOf entity (RenderTexture Sun)
    updatePosOf entity  (Position 200 150)
