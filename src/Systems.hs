module Systems
        (
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

import           Control.Lens
import qualified Data.Map.Strict            as Map

import           Components.Acceleration
import           Components.Bounds
import           Components.MaxSpeed
import           Components.Position
import           Components.Velocity

import           Components.Collisions
import           Components.Input

---------- SYSTEMS ------------------


processVelocities :: Float -> State World ()
processVelocities dt = do
           entity_velocities <- use velocities
           mapM_ (\(k, Velocity dx' dy')-> (do
                          maybePos <- positionOf k
                          let newPos = (\(Position x' y') -> Position (x'+dx'*dt) (y'+dy'*dt))<$>maybePos
                          sequenceA $ updatePosOf k <$>newPos
                                           )
                  )(Map.toList entity_velocities)


processAccelerations :: Float -> State World ()
processAccelerations dt = do
           entity_accelerations <- use accelerations
           mapM_ (\(k, Acceleration ax' ay')-> (do
                          maybeVel <- velocityOf k
                          let newVel = (\(Velocity dx' dy') -> Velocity (dx'+ax'*dt) (dy'+ay'*dt))<$>maybeVel
                          sequenceA $ updateVelOf k <$>newVel
                                           )
                  )(Map.toList entity_accelerations)

processSpeedLimits :: State World ()
processSpeedLimits = do
           entity_maxSpeeds <- use maxSpeeds
           mapM_ (\(k, MaxSpeed mdx' mdy')-> (do
                          maybeVel <- velocityOf k
                          let newVel = (\(Velocity dx' dy') -> Velocity (bound dx' mdx') (bound dy' mdy'))<$>maybeVel
                          sequenceA $ updateVelOf k <$>newVel
                                           )
                  )(Map.toList entity_maxSpeeds)
            where bound v maxV
                        | v > maxV   =  maxV
                        | v < -maxV  = -maxV
                        | otherwise =  v

processWorldBoundaries :: State World ()
processWorldBoundaries = do
                    (w2, h2) <- ((/2)***(/2)) <$> use (dimensions.wh)
                    entity_positions <-  use positions
                    mapM_ (\(k, Position x' y')->
                                         updatePosOf k (uncurry Position $ (constrain w2 *** constrain h2) (x',y')   )
                          ) (Map.toList entity_positions)

                    where
                         constrain bound v
                                    | v < -bound = bound
                                    | v > bound  = -bound
                                    | otherwise  = v


processInput :: Input -> State World ()
processInput input = do
                 inputProcs <- use inputProcessors
                 mapM_ (\(k, f)-> f k input) (Map.toList inputProcs)



processCollision :: State World ()
processCollision = do
            collidersToCheck <- Map.toList <$> use colliders
            collidablesToCheck <- Map.toList <$> use collidables

            mapM_ collisionCheck [(collider, collidable) | collider <- collidersToCheck, collidable <- collidablesToCheck]


            where collisionCheck ((k, _), (_, v1))
                   | v1 == Platform
                   = do
                       mfallSpeed <- liftM (^. dx)<$> velocityOf k -- ritorna Maybe Float
                       pos <- positionOf k
                       bo <- boundsOf k
                       let anchor = anchorEdge pos bo

                       undefined
                       {-if mfallSpeed < -1 then-}
                            {-undefined-}
                       {-else-}
                            {-undefined-}

                   | v1 == Tile
                   = undefined

                   | otherwise
                   = undefined

                  anchorEdge pos bo = (\(Position x1 y1) (Bounds w h) -> ((x1-w/2, y1-h/2),(x1+w/2, y1-h/2))) <$> pos <*> bo

