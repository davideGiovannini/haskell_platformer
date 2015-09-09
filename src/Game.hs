{-# LANGUAGE TemplateHaskell #-}
module Game (
        GameState,
        initialState,
        width,
        height,
        deltaTime,
        totalTime,
        updateDT,
        viewPort,
        background,
        world
        )
where

import           Control.Lens                 hiding (Level)
import           Control.Monad.State.Strict
import           Data.Fixed                   (div', mod')
import qualified Data.Map.Strict              as Map (lookup)
import           Data.Maybe                   (fromJust)

import           Graphics.Gloss.Data.ViewPort

import           Entities.Fly
import           Entities.Player


import           Entities
import           Systems                      hiding (update)

import           Game.Levels                  (initialLevel)
import           Game.Viewport


import           Components.Input

import Game.Levels.Backgrounds (Background, desertBackground)

------------------------------------------------

width, height :: Int
width  = 640
height = 480

deltaTime :: Double
deltaTime = 0.01

deltaTimeF :: Float
deltaTimeF = 0.01

gravity :: (Float, Float)
gravity = (0, -4800)


data GameState = GameState {
                  _totalTime :: Double,

                  _world     :: World,
                  _viewPort  :: ViewPort,
                  _player    :: Int,
                  _background :: Background

                 }

makeLenses ''GameState


initialState :: GameState
initialState = let (player', world') = runState (do
                                                  newFly  (10,0) (20,0)
                                                  initialLevel
                                                  newPlayer  (100,100) gravity
                                               )
                                      emptyWorld
               in
               GameState 0 world' viewPortInit  (player' ^. getId) desertBackground



updateDT :: Double -> Input -> State GameState Double
updateDT acc input = do
                        replicateM_ (div' acc deltaTime) (update input)
                        return $ mod' acc deltaTime

update :: Input -> State GameState ()
update input = do
           totalTime += deltaTime

           world %= execState (do
                                 forAllEntities $ processInput input
                                 forAllEntities $ processAccelerations deltaTimeF
                                 forAllEntities processSpeedLimits
                                 forAllEntities $ processVelocities deltaTimeF
                                 forAllEntities processCollision
                                 forAllEntities processWorldBoundaries
                              )

           playerId <- use player
           entitiesMap <-  use $ world.entities
           viewPort %= followEntity deltaTimeF (fromJust $ Map.lookup playerId entitiesMap)


           {-currPlayer <- use player-}
           {-player .= execState (Player.update input deltaTimeF) currPlayer-}

           {-let useInCollision block = case block ^. blockType of Box  -> True; SandTop -> True; _ -> False-}
           {-solidtiles <- gets $ \gamestate -> Vector.filter useInCollision (gamestate ^.  level.tiles)-}

           {-player %= execState (collision solidtiles (currPlayer ^. position.y))-}

           {-updatedPlayer <- use player-}
           {-viewport %= followPlayer deltaTimeF updatedPlayer-}




-- TODO refactor this function like the one above
{-collision :: Vector Block -> Float -> State Player ()-}
{-collision solidtiles oldY = do-}
        {-fallingSpeed <-  use $ velocity.dy-}
        {-anchorPoint  <- Player.anchorPoint <$> get-}

        {-if fallingSpeed < 0  && any (shouldStopFall anchorPoint) solidtiles then-}
            {-Player.landOn oldY-}
        {-else-}
            {-Player.onGround    .= False-}
    {-where-}
          {-shouldStopFall :: (Float, Float) -> Block -> Bool-}
          {-shouldStopFall anchor block = topOfSquare anchor (block ^. position.xy) (block ^. bounds.wh)-}




{-insideSquare :: (Float, Float) -> (Float, Float) -> Float -> Bool-}
{-insideSquare (x, y) (sx, sy) w = x > sx-w2 && x < sx+w2 && y > sy-w2 && y < sy+w2-}
                {-where w2 = w/2-}






