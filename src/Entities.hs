{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Entities
    (
        Entity,
        World,
        InputProcessor,
        (<==),(|.|),

        entities,  -- TODO remove unnecessary power to users of this API

        dimensions,

        emptyWorld,
        newEntity,
        getNewEntity,
        updateEntity,
        removeEntity,

        bounds,
        getId,
        acceleration,
        collidable,
        collider,
        inputProcessor,
        jumpAbility,
        maxSpeed,
        velocity,
        position,
        renderable,
        friction

    )
where

import           Control.Lens

import qualified Data.Map.Strict                   as Map (Map, delete, empty, insert,
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
data Entity = Entity {
                         _getId          :: Int,

                         _position       :: Maybe Position,
                         _velocity       :: Maybe Velocity,
                         _acceleration   :: Maybe Acceleration,
                         _bounds         :: Maybe Bounds,
                         _jumpAbility    :: Maybe JumpAbility,
                         _inputProcessor :: Maybe InputProcessor,
                         _renderable     :: Maybe Renderable,
                         _maxSpeed       :: Maybe MaxSpeed,
                         _collider       :: Maybe Collider,
                         _collidable     :: Maybe Collidable,
                         _friction       :: Maybe Friction
                        }

instance Show Entity where
    show e = show $ _getId e

instance Eq Entity where
    a == b = _getId a == _getId b

instance Ord Entity where
    a > b  = _getId a > _getId b
    a <= b = _getId a <= _getId b


(<==) :: forall components. ASetter Entity Entity (Maybe components) (Maybe components) -> components -> Entity -> Entity
lns <== val = lns .~ Just val

(|.|) :: (Entity -> Entity) -> (Entity -> Entity) -> Entity -> Entity
infixl 8 |.|
f |.| g = f . g

--------- World DEFINITION


data World = World {
                   _maxID      :: Int,
                   _dimensions :: Bounds,
                   _entities   :: Map.Map Int Entity

                 }

makeLenses ''World
makeLenses ''Entity

emptyWorld :: World
emptyWorld = World 0 emptyBounds Map.empty





getNewEntity :: (Entity -> Entity) -> State World Entity
getNewEntity settings =  do
    intId <- use maxID
    maxID += 1
    let entity = Entity intId
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing

    entities %= Map.insert intId (settings entity)

    return entity

newEntity :: (Entity -> Entity) -> State World ()
newEntity settings = do
           _ <- getNewEntity settings
           return ()



updateEntity :: Entity ->  State World ()
updateEntity e = entities %= Map.insert (_getId e) e


removeEntity :: Entity -> State World ()
removeEntity entity = do
      let eId = _getId entity
      entitiesSet <- use entities
      when (eId `Map.member` entitiesSet) (entities %= Map.delete eId)
      --TODO maybe do something if trying to remove non existent entities


