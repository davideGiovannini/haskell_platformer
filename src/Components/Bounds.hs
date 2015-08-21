{-# LANGUAGE TemplateHaskell #-}
module Components.Bounds where

import           Control.Arrow ((&&&))
import           Control.Lens

data Bounds = Bounds {
                                _width  :: Float,
                                _height :: Float
                              }deriving(Show, Eq)


wh :: Lens' Bounds (Float, Float)
wh = lens (_width &&& _height ) (\size (nw, nh) -> size {_width = nw, _height = nh})

makeLenses ''Bounds
