module Entities.Fly

    where

import           Components.Position
import           Components.Renderable
import           Components.Velocity

import           Entities

import           Resources                  (Animation (Fly))

import           Control.Monad.State.Strict

import Control.Lens((&))


newFly :: (Float, Float) -> (Float, Float)-> State World ()
newFly pos vel = do
    entity <- newEntity

    updateEntity $ entity & position   -| uncurry Position pos
                          & velocity   -| uncurry Velocity vel
                          & renderable -| RenderAnim 2 Fly









