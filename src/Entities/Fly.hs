module Entities.Fly

    where

import           Components.Position
import           Components.Renderable
import           Components.Velocity
import           Components.Direction

import           Entities

import           Resources                  (Animation (Fly))

import           Control.Monad.State.Strict



newFly :: (Float, Float) -> (Float, Float)-> State World ()
newFly pos vel =
    newEntity $ position   <== uncurry Position pos
            |.| velocity   <== uncurry Velocity vel
            |.| renderable <== (\_ -> RenderAnim 2 Fly)
            |.| direction  <== InvertedSpeedDirection








