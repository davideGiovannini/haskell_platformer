{-# LANGUAGE TemplateHaskell #-}
module Game.Entities.Fly

    where

import Control.Lens
import Game.Entities



data Fly = Fly {
                _flyPosition     :: ComponentPosition,
                _flyVelocity     :: ComponentVelocity,
                _flyAcceleration :: ComponentAcceleration
               }deriving(Show)

makeLenses ''Fly

instance BasicEntity Fly where
    position = flyPosition

instance PhysicalEntity Fly where
    velocity = flyVelocity
    acceleration = flyAcceleration


newFly :: (Float, Float) -> (Float, Float)-> Fly
newFly pos vel = Fly (uncurry Position pos) (uncurry Velocity vel) (Acceleration 0 0)






