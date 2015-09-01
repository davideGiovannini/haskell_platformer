{-# LANGUAGE TemplateHaskell #-}
module Components.MaxSpeed
    where


import           Control.Arrow              ((&&&))
import           Control.Lens



data MaxSpeed = MaxSpeed{
                                  _mdx :: Float,
                                  _mdy :: Float
                                 } deriving(Show, Eq)

makeLenses ''MaxSpeed

mdxy :: Lens' MaxSpeed (Float, Float)
mdxy = lens (_mdx &&& _mdy ) (\vel (ndx, ndy) -> vel {_mdx = ndx, _mdy = ndy})

