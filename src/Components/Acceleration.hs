{-# LANGUAGE TemplateHaskell #-}
module Components.Acceleration where

import           Control.Arrow ((&&&))
import           Control.Lens

data Acceleration = Acceleration{
                                  _ax :: Float,
                                  _ay :: Float
                                 } deriving(Show, Eq)

axy :: Lens' Acceleration (Float, Float)
axy = lens (_ax &&& _ay ) (\acc (nax, nay) -> acc {_ax = nax, _ay = nay})


makeLenses ''Acceleration
