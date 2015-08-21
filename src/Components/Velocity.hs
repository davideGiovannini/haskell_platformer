{-# LANGUAGE TemplateHaskell #-}
module Components.Velocity
    where


import           Control.Arrow              ((&&&))
import           Control.Lens



data Velocity = Velocity{
                                  _dx :: Float,
                                  _dy :: Float
                                 } deriving(Show, Eq)

makeLenses ''Velocity

dxy :: Lens' Velocity (Float, Float)
dxy = lens (_dx &&& _dy ) (\vel (ndx, ndy) -> vel {_dx = ndx, _dy = ndy})

