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
import           Components.Collisions
import           Components.Position
import           Components.Renderable

tileSize :: Float
tileSize = 70


frictionValue :: Float
frictionValue = 0.7

-------------------- FUNCTIONS ------------


newBlock :: Texture -> Bool -> (Float, Float)-> State World ()
newBlock bType solid (x1, y1) = do

    let collisions = if solid then collidable <== Platform |.| friction <== Friction frictionValue
                     else id

    newEntity $ position   <== Position x1 y1
            |.| renderable <== RenderTexture bType
            |.| bounds     <== Bounds tileSize tileSize
            |.| collisions



sandCenter,sandTop, box, cactus :: (Float, Float)-> State World ()
sandCenter = newBlock Sandcenter False
sandTop    = newBlock SandTop True
box        = newBlock Box True
cactus     = newBlock Cactus False



