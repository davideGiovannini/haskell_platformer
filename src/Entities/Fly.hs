module Entities.Fly

    where

import           Components.Position
import           Components.Renderable
import           Components.Velocity

import           Entities

import           Resources                  (Animation (Fly))

import           Control.Monad.State.Strict



newFly :: (Float, Float) -> (Float, Float)-> State World ()
newFly pos vel = do
    entity <- newEntity

    updatePosOf entity (uncurry Position pos)
    updateVelOf entity (uncurry Velocity vel)
    updateRenderOf entity (RenderAnim 2 Fly)








