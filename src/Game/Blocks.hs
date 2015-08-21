module Game.Blocks
(
    sandCenter,
    cactus,
    box,
    sandTop
    )
where

import           Entities
import           Resources

import           Control.Monad.State.Strict


import           Components.Bounds
import           Components.Position
import           Components.Renderable

tileSize :: Float
tileSize = 70


-------------------- FUNCTIONS ------------


newBlock :: Texture -> (Float, Float)-> State World ()
newBlock bType (x1, y1) = do
    entity <- newEntity

    updatePosOf entity (Position x1 y1)
    updateRenderOf entity (RenderTexture bType)
    updateBoundsOf entity (Bounds tileSize tileSize)



sandCenter,sandTop, box, cactus :: (Float, Float)-> State World ()
sandCenter = newBlock Sandcenter
sandTop    = newBlock SandTop
box        = newBlock Box
cactus     = newBlock Cactus



