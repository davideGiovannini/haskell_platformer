module Game.Levels.Backgrounds where

import Graphics.Gloss (Color, makeColorI)

data BackgroundElement = Sun
                       | Clouds1
                       | Hills1
                       | Hills2
                       | Empty
                       deriving (Show, Eq)

data Background = Background{
                             _fillColor :: Color,
                             _statics   :: BackgroundElement,
                             _parallax1 :: BackgroundElement,
                             _parallax2 :: BackgroundElement,
                             _parallax3 :: BackgroundElement
                            } deriving Show


desertBackground :: Background
desertBackground = Background (makeColorI 213 237 247 255) Sun Clouds1 Hills2 Hills1
