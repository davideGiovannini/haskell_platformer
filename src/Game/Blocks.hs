module Game.Blocks
(
    sandCenter,
    cactus,
    box,
    sandTop
    )
where

import           Control.Lens               ((%=))
import           Entities
import           Resources

import           Control.Monad.State.Strict

import           Data.Vector                (cons)

import           Components.Bounds
import           Components.Position
import           Components.Renderable

tileSize :: Float
tileSize = 70


-------------------- FUNCTIONS ------------


newBlock :: Texture -> Int -> (Float, Float)-> State World ()
newBlock bType intId (x1, y1) = do
    let entity = Entity intId
    entities %= cons entity

    updatePosOf entity (Position x1 y1)
    updateRenderOf entity (RenderTexture bType)
    updateBoundsOf entity (Bounds tileSize tileSize)




sandCenter,sandTop, box, cactus :: Int -> (Float, Float)-> State World ()
sandCenter = newBlock Sandcenter
sandTop    = newBlock SandTop
box        = newBlock Box
cactus     = newBlock Cactus



