{-# LANGUAGE TemplateHaskell #-}
module Game.Blocks
(
    BlockType(..),
    Block,
    blockAt,
    blockType
    )
where

import           Control.Lens
import           Game.Entities (BasicEntity, CollisionEntity,
                                ComponentBounds (..), ComponentPosition (..),
                                bounds, position)

tileSize :: Float
tileSize = 70

--------------- DATA DEFINITION ----------------

data BlockType = SandCenter
               | SandTop
               | Box
               |Cactus
               deriving (Show, Eq, Ord)


data Block = Block {
                    _blockType     :: BlockType,
                    _blockPosition :: ComponentPosition,
                    _blockSize     :: ComponentBounds
                   }
                   deriving (Show, Eq)

makeLenses ''Block

instance BasicEntity Block where
    position = blockPosition

instance CollisionEntity Block where
    bounds = blockSize

-------------------- FUNCTIONS ------------


blockAt :: BlockType -> (Float, Float) -> Block
blockAt bType (x1, y1) = Block bType (Position x1 y1) (Bounds tileSize tileSize)
