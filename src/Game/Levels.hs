{-# LANGUAGE TemplateHaskell #-}
module Game.Levels
   (
       Level,
       bounds,
       tiles,
       initialLevel
   )
where




import           Control.Lens hiding (Level)
import           Data.Vector  (Vector)
import qualified Data.Vector  as Vector
import           Game.Blocks

data Level = Level {
                     _bounds :: (Float, Float),
                     _tiles  :: Vector Block
                   }

makeLenses ''Level



initialLevel :: Level
initialLevel = Level (4399, 600) (Vector.fromList $ [
                                        Box      (-355,5),
                                        Box      (-355,-65),

                                        Box      (355,-135),

                                        Box      (1055,-135),
                                        Box      (1125,-65),
                                        Box      (1195,-5)
                                        ] ++ concat[
                                         drawSquare x (-205) | x <- [5,75..1800]]
                                         ++ concat
                                         [drawSquare x (-135) | x <- [-565,(-495)..(-355)]]
                                         ++ [SandCenter(x,-275) | x <- [-565,(-495)..(-285)]]
                                        )



drawSquare :: Float -> Float -> [Block]
drawSquare x y = [SandTop (x, y), SandTop(x+70, y), SandCenter(x,y-70), SandCenter(x+70, y-70)]



