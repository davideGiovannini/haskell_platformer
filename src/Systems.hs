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

import           Components.Collisions
import           Components.Input

---------- Utility functions ------------------
has :: forall a . Entity -> Getting (Maybe a) Entity (Maybe a) -> Bool
has entity attr  = isJust (entity ^. attr)

from :: forall a . Getting (Maybe a) Entity (Maybe a) -> Entity -> a
from attr entity = fromJust (entity ^. attr)

update :: forall components. Entity -> ASetter Entity Entity (Maybe components) (Maybe components) -> components -> State World()
update entity attr value = updateEntity (entity & attr .~ Just value)



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
processCollision e = return ()
            {-collidersToCheck <- Map.toList <$> use colliders-}
            {-collidablesToCheck <- Map.toList <$> use collidables-}

            {-mapM_ collisionCheck [(collider, collidable) | collider <- collidersToCheck, collidable <- collidablesToCheck]-}


            {-where collisionCheck ((k, _), (_, v1))-}
                   {-| v1 == Platform-}
                   {-= do-}
                       {-mfallSpeed <- liftM (^. dx)<$> velocityOf k -- ritorna Maybe Float-}
                       {-pos <- positionOf k-}
                       {-bo <- boundsOf k-}
                       {-let anchor = anchorEdge pos bo-}

                       {-undefined-}
                       {-[>if mfallSpeed < -1 then<]-}
                            {-[>undefined<]-}
                       {-[>else<]-}
                            {-[>undefined<]-}

                   {-| v1 == Tile-}
                   {-= undefined-}

                   {-| otherwise-}
                   {-= undefined-}

                  {-anchorEdge pos bo = (\(Position x1 y1) (Bounds w h) -> ((x1-w/2, y1-h/2),(x1+w/2, y1-h/2))) <$> pos <*> bo-}

