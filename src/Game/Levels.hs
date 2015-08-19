{-# LANGUAGE TemplateHaskell #-}
module Game.Levels
   (
       Level,
       levelBounds,
       tiles,
       background,
       initialLevel
   )
where



import           Control.Lens     hiding (Level)
import           Data.Vector      (Vector)
import qualified Data.Vector      as Vector
import           Game.Blocks      (Block, BlockType (..), blockAt)

import           Game.Backgrounds (Background, desertBackground)

data Level = Level {
                     _background  :: Background,
                     _levelBounds :: (Float, Float),
                     _tiles       :: Vector Block
                   }

makeLenses ''Level



initialLevel :: Level
initialLevel = Level desertBackground (4399, 600) (Vector.fromList $ [
                                        Box     `blockAt` (-355,5),
                                        Box     `blockAt` (-355,-65),

                                        Box     `blockAt` (355,-135),
                                        Cactus  `blockAt` (215, -135),

                                        Box    `blockAt`  (1055,-135),
                                        Box    `blockAt`  (1125,-65),
                                        Box    `blockAt`  (1195,-5),
                                        Box    `blockAt`  (1265,65),
                                        Box    `blockAt`  (1135,135)
                                        ] ++ concat[
                                         drawSquare x (-205) | x <- [5,75..1800]]
                                         ++ concat
                                         [drawSquare x (-135) | x <- [-565,(-495)..(-355)]]
                                         ++ [SandCenter `blockAt` ( x,-275) | x <- [-565,(-495)..(-285)]]
                                        )



drawSquare :: Float -> Float -> [Block]
drawSquare x y = [blockAt SandTop (x, y), blockAt SandTop(x+70, y), blockAt SandCenter(x,y-70), blockAt SandCenter(x+70, y-70)]



