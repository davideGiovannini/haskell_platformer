module Game.Levels
   (
       initialLevel
   )
where



import Control.Monad.State.Strict
import Entities(World)
import Game.Blocks
import Entities.Fly(newFly)

import Game.Levels.Backgrounds

{-data Level = Level -}
                     {-_background  :: Background,-}
                     {-_levelBounds :: (Float, Float)-}


{-makeLenses ''Level-}

{-initialLevel :: Level-}
{-initialLevel = Level desertBackground (4399, 600) (Vector.fromList $ [-}

initialLevel :: State World ()
initialLevel = do
                desertBackground

                box (-355,5)
                box (-355,-65)

                box (355,-135)
                cactus (215, -135)

                box (1055,-135)
                box (1125,-65)
                box (1195,-5)
                box (1265,65)
                box (1135,135)

                sequence_[drawSquare x (-205) | x <- [5,75..1800]]

                sequence_[drawSquare x (-135) | x <- [-565,(-495)..(-355)]]

                sequence_ [sandCenter( x,-275) | x <- [-565,(-495)..(-285)]]

                sequence_ [ newFly (0,0) (-250,0),
                            newFly (-140,40) (260,0),
                            newFly (-240,80) (265,0),
                            newFly (-180,-20) (-240,0),
                            newFly (140,-60) (270,0)
                          ]



drawSquare :: Float -> Float -> State World ()
drawSquare x y = sequence_ [sandTop (x, y), sandTop (x+70, y), sandCenter (x,y-70), sandCenter (x+70, y-70)]



