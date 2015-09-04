{-# LANGUAGE TemplateHaskell #-}
module Entities
    (
        Entity,
        World,
        InputProcessor,

        entities,  -- TODO remove unnecessary power to users of this API
        positions,
        velocities,
        accelerations,
        boundaries,
        jumpInfos,
        inputProcessors,
        renderables,
        maxSpeeds,
        collidables,
        colliders,

        dimensions,

        emptyWorld,
        newEntity,
        removeEntity,

        positionOf,
        updatePosOf,

        velocityOf,
        updateVelOf,

        accelOf,
        updateAccOf,

        boundsOf,
        updateBoundsOf,

        renderOf,
        updateRenderOf,

        jumpAbilityOf,
        updateJumpAbOf,

        maxSpeedOf,
        updateMaxSpeedOf,

        inputProcessorOf,
        updateInputProcessorOf,

    )
where

import           Control.Lens

import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set (Set, delete, empty, insert,
                                                    member)

import           Control.Monad.State.Strict

import           Components.Acceleration
import           Components.Bounds
import           Components.Input
import           Components.JumpAbility
import           Components.MaxSpeed
import           Components.Position
import           Components.Renderable
import           Components.Velocity

import           Components.Collisions

type InputProcessor = Entity -> Input -> State World ()

--------- Entity Definition
newtype Entity = Entity {
                         getId :: Int
                        } deriving (Show)

instance Eq Entity where
    a == b = getId a == getId b

instance Ord Entity where
    a > b  = getId a > getId b
    a <= b = getId a <= getId b

--------- World DEFINITION

type Map = Map.Map Entity

data World = World {
                   _maxID           :: Int,
                   _dimensions      :: Bounds,
                   _entities        :: Set.Set Entity,

                   _positions       :: Map Position,
                   _velocities      :: Map Velocity,
                   _accelerations   :: Map Acceleration,
                   _boundaries      :: Map Bounds,
                   _jumpInfos       :: Map JumpAbility,
                   _inputProcessors :: Map InputProcessor,
                   _renderables     :: Map Renderable,
                   _maxSpeeds       :: Map MaxSpeed,
                   _colliders       :: Map Collider,
                   _collidables     :: Map Collidable
                 }

makeLenses ''World

emptyWorld :: World
emptyWorld = World 0 emptyBounds Set.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty


newEntity :: State World Entity
newEntity =  do
    intId <- use maxID
    maxID += 1
    let entity = Entity intId
    entities %= Set.insert entity

    return entity

removeEntity :: Entity -> State World ()
removeEntity entity = do
      entitiesSet <- use entities
      when (entity `Set.member` entitiesSet) (entities %= Set.delete entity)
      --TODO maybe do something if trying to remove non existent entities

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


----- JumpAbility

jumpAbilityOf :: Entity -> State World (Maybe JumpAbility)
jumpAbilityOf e = uses jumpInfos $ Map.lookup e

updateJumpAbOf :: Entity -> JumpAbility -> State World ()
updateJumpAbOf e val = jumpInfos %= Map.insert e val


----- MaxSpeed

maxSpeedOf :: Entity -> State World (Maybe MaxSpeed)
maxSpeedOf e = uses maxSpeeds $ Map.lookup e

updateMaxSpeedOf :: Entity -> MaxSpeed -> State World ()
updateMaxSpeedOf e val = maxSpeeds %= Map.insert e val


----- InputProcessor

inputProcessorOf :: Entity -> State World (Maybe InputProcessor)
inputProcessorOf e = uses inputProcessors $ Map.lookup e

updateInputProcessorOf :: Entity -> InputProcessor -> State World ()
updateInputProcessorOf e val = inputProcessors %= Map.insert e val









