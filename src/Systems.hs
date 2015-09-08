{-# LANGUAGE RankNTypes #-}
module Systems
        (
            has, from, update,
        forAllEntities,
        processSpeedLimits,
        processVelocities,
        processAccelerations,

        processWorldBoundaries,
        processInput,

        processCollision
        )
where

import           Control.Arrow              ((***))
import           Control.Monad.State.Strict
import           Entities

import           Control.Lens               hiding (from, has)
import           Data.Maybe                 (fromJust, isJust)

import           Components.Acceleration
import           Components.Bounds
import           Components.MaxSpeed
import           Components.Position
import           Components.Velocity
import           Components.JumpAbility

import           Components.Collisions
import           Components.Input

---------- Utility functions ------------------
has :: forall a . Entity -> Getting (Maybe a) Entity (Maybe a) -> Bool
has entity attr  = isJust (entity ^. attr)

from :: forall a . Getting (Maybe a) Entity (Maybe a) -> Entity -> a
from attr entity = fromJust (entity ^. attr)

update :: forall components. Entity -> ASetter Entity Entity (Maybe components) (Maybe components) -> components -> State World()
update entity attr value = updateEntity (entity & attr -| value)



------ Systems


forAllEntities :: (Entity -> State World ()) -> State World ()
forAllEntities action = do
                  entitiesSet <- use entities
                  mapM_ action entitiesSet

processVelocities :: Float -> Entity -> State World ()
processVelocities dt entity =
            when (entity `has` position && entity `has` velocity)
                 (do
                    let (Velocity dx' dy') = velocity `from` entity
                        newPos = (\(Position x' y') -> Position (x'+dx'*dt) (y'+dy'*dt)) $ position `from` entity

                    update entity position newPos
                 )


processAccelerations :: Float -> Entity -> State World ()
processAccelerations dt entity =
            when (entity `has` acceleration && entity `has` velocity)
                 (do
                    let (Acceleration ax' ay') = acceleration `from` entity
                        newVel = (\(Velocity dx' dy') -> Velocity (dx'+ax'*dt) (dy'+ay'*dt)) $ velocity `from` entity

                    update entity velocity newVel
                 )

processSpeedLimits :: Entity -> State World ()
processSpeedLimits entity =
            when (entity `has` maxSpeed && entity `has` velocity)
                 (do
                    let (MaxSpeed mdx' mdy') = maxSpeed `from` entity
                        newVel = (\(Velocity dx' dy') -> Velocity (bound dx' mdx') (bound dy' mdy')) $ velocity `from` entity

                    update entity velocity newVel
                 )
            where bound v maxV
                        | v > maxV   =  maxV
                        | v < -maxV  = -maxV
                        | otherwise =  v

processWorldBoundaries :: Entity -> State World ()
processWorldBoundaries entity = do
                    (w2, h2) <- ((/2)***(/2)) <$> use (dimensions.wh)
                    when (entity `has` position)
                         (do
                            let newPos = (\(Position x' y') -> uncurry Position $ (constrain w2 *** constrain h2) (x', y')) $ position `from` entity
                            update entity position newPos
                         )
                    where
                         constrain bound v
                                    | v < -bound = bound
                                    | v > bound  = -bound
                                    | otherwise  = v


processInput :: Input -> Entity -> State World ()
processInput input entity =
            when (entity `has` inputProcessor)
                 (
                  (inputProcessor `from` entity) entity input
                 )



processCollision :: Entity -> State World ()
processCollision entity =
                when (entity `has` bounds  &&
                      entity `has` collider &&
                      entity `has` velocity &&
                      entity `has` position)
                     (
                         forAllEntities (\ entity2 ->
                                             when (entity2 `has` bounds &&
                                                   entity2 `has` collidable &&
                                                   entity2 `has` position &&
                                                   entity2 `has` friction)
                                                  (
                                                      collisionCheck entity entity2
                                                  )
                                        )
                     )
            where collisionCheck entityA entityB = do
                       let fallSpeed = (velocity `from` entityA) ^. dy
                           posA = position `from` entityA
                           boundsA = bounds `from` entityA
                           speedA = velocity `from` entityA

                           anchorA = anchorEdge posA boundsA
                           posB = position `from` entityB
                           boundsB = bounds `from` entityB
                           frict = getFriction $ friction `from` entityB


                       when (fallSpeed < -1 && topOfSquare anchorA posB boundsB)
                            ( do
                                let by = (posB ^. y) + (boundsB ^. height)/2 + (boundsA ^. height)/2
                                    jumpInfos = if entityA `has` jumpAbility then
                                                     jumpAbility .~ Just((jumpAbility `from` entityA) {_onGround = True})
                                                else
                                                     id
                                updateEntity (entityA & position -|(posA {_y = by})
                                                      & velocity -|(speedA {_dy = 0, _dx = _dx speedA*frict})
                                                      & jumpInfos
                                             )
                            )



                  anchorEdge (Position x1 y1) (Bounds w h) = ((x1-w/2, y1-h/2), (x1+w/2, y1-h/2))

                  topOfSquare ((u, v), (u1, v1)) (Position sx sy) (Bounds w h) = u > sx-w2 && u < sx+w2 && v > sy+h4 && v < sy+h2 ||
                                                                                 u1 > sx-w2 && u1 < sx+w2 && v1 > sy+h4 && v1 < sy+h2
                                                                                 where w2 = w/2
                                                                                       h2 = h/2
                                                                                       h4 = h/4
