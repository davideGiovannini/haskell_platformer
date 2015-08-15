{-# LANGUAGE TemplateHaskell #-}
module Game.Blocks where

import           Control.Lens

blockSize :: Float
blockSize = 70

data Block = SandCenter{
                    _pos :: (Float, Float)
                  }
           | SandTop{
                    _pos :: (Float, Float)
                  }
           | Box{
                    _pos :: (Float, Float)
                  }
           deriving (Show, Eq)

makeLenses ''Block
