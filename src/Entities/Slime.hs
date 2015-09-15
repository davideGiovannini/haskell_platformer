
module Entities.Slime
(newSlime)
    where

import           Components.Position
import           Components.Renderable
import           Components.Velocity
import           Components.Acceleration
import           Components.Direction
import           Components.Collisions
import           Components.Bounds
import           Components.MaxSpeed

import           Entities

import           Resources                  (Animation (Slime))

import           Control.Monad.State.Strict

import Control.Lens ((&),(+~))
import Systems

slimeBounds, slimeMaxSpeed :: (Float, Float)
slimeBounds = (50, 28)
slimeMaxSpeed    = (200, 550)

walkSpeed :: Float
walkSpeed = 60


newSlime :: (Float, Float) -> (Float, Float) -> (Float, Float)-> State World ()
newSlime pos vel gravity =
    newEntity $ position     <== uncurry Position pos
            |.| velocity     <== uncurry Velocity vel
            |.| acceleration <== uncurry Acceleration gravity
            |.| collider       <== Collider
            |.| bounds         <== uncurry Bounds slimeBounds
            |.| renderable   <== (\_ -> RenderAnim 2 Slime)
            |.| direction    <== InvertedSpeedDirection
            |.| maxSpeed    <== uncurry MaxSpeed  slimeMaxSpeed

            |.| inputProcessor <== slimeInputProcessor



slimeInputProcessor :: InputProcessor
slimeInputProcessor entity _ =
                    when (entity `has` velocity ) (do
                       let vel = velocity `from` entity
                           horizontalSpeedFun = velocity <== (vel & dx +~ walkSpeed)

                       updateEntity $ entity & horizontalSpeedFun
                       )



