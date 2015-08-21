{-# LANGUAGE TemplateHaskell #-}
module Entities
where

import           Control.Lens

import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector(Vector, empty)

import Control.Monad.State.Strict


import Components.Position
import Components.Velocity
import Components.Acceleration
import Components.Bounds
import Components.Renderable

--- BASIC ENTITY with Position  --------------------

newtype Entity = Entity {
                         getId :: Int
                        }

instance Eq Entity where
    a == b = getId a == getId b

instance Ord Entity where
    a > b  = getId a > getId b
    a <= b = getId a <= getId b








data ComponentJump = Jump {
                            _onGround  :: Bool,
                            _jumpTimer :: Int
                          }


data ComponentInput = Input




makeLenses ''ComponentJump



type Map = Map.Map Entity

data World = World {
                   _entities      :: Vector.Vector Entity,
                   _positions     :: Map Position,
                   _velocities    :: Map Velocity,
                   _accelerations :: Map Acceleration,
                   _boundaries    :: Map Bounds,
                   _jumpInfos     :: Map ComponentJump,
                   _inputs        :: Map ComponentInput,
                   _renderables   :: Map Renderable
                 }

makeLenses ''World

emptyWorld :: World
emptyWorld = World Vector.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty


----- POSITION

positionOf :: Entity ->  State World (Maybe Position)
positionOf e = uses positions $ Map.lookup e

updatePosOf :: Entity -> Position -> State World ()
updatePosOf e val = positions %= Map.insert e val

----- VELOCITY

velocityOf :: Entity ->  State World (Maybe Velocity)
velocityOf e = uses velocities $ Map.lookup e

updateVelOf :: Entity -> Velocity -> State World ()
updateVelOf e val = velocities %= Map.insert e val

----- ACCELERATION

accelOf :: Entity ->  State World (Maybe Acceleration)
accelOf e = uses accelerations $ Map.lookup e

updateAccOf :: Entity -> Acceleration -> State World ()
updateAccOf e val = accelerations %= Map.insert e val

----- Bounds

boundsOf :: Entity ->  State World (Maybe Bounds)
boundsOf e = uses boundaries $ Map.lookup e

updateBoundsOf :: Entity -> Bounds -> State World ()
updateBoundsOf e val = boundaries %= Map.insert e val

----- Renderable

renderOf :: Entity -> State World (Maybe Renderable)
renderOf e = uses renderables $ Map.lookup e

updateRenderOf :: Entity -> Renderable -> State World ()
updateRenderOf e val = renderables %= Map.insert e val



---------- SYSTEMS ------------------


updateVelocities :: Float -> State World ()
updateVelocities dt = do
           entity_velocities <- use velocities
           mapM_ (\(k, Velocity dx' dy')-> (do
                          maybePos <- positionOf k
                          let newPos = (\(Position x' y') -> Position (x'+dx'*dt) (y'+dy'*dt))<$>maybePos
                          sequenceA $ updatePosOf k <$>newPos
                                           )
                  )(Map.toList entity_velocities)


