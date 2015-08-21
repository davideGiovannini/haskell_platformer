{-# LANGUAGE TemplateHaskell #-}
module Components.Position
    where


import           Control.Arrow ((&&&))
import           Control.Lens


data Position = Position{
                                   _x :: Float,
                                   _y :: Float
                                 } deriving(Show, Eq)

xy :: Lens' Position (Float, Float)
xy = lens (_x &&& _y ) (\pos (nx, ny) -> pos {_x = nx, _y = ny})

makeLenses ''Position


