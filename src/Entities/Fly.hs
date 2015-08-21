module Entities.Fly

    where

import           Components.Position
import           Components.Velocity
import           Components.Renderable

import           Control.Lens               ((%=))
import           Entities

import           Resources                  (Animation (Fly))

import           Control.Monad.State.Strict

import           Data.Vector                (cons)


newFly :: Int -> (Float, Float) -> (Float, Float)-> State World ()
newFly intId pos vel = do
    let entity = Entity intId
    entities %= cons entity

    updatePosOf entity (uncurry Position pos)
    updateVelOf entity (uncurry Velocity vel)
    updateRenderOf entity (RenderAnim 2 Fly)








