{-# LANGUAGE TemplateHaskell #-}
module Game (
        GameState,
        initialState,
        width,
        height,
        deltaTime,
        totalTime,
        updateDT,
        world
        )
where

import           Control.Lens               hiding (Level)
import           Control.Monad.State.Strict
import           Data.Fixed                 (div', mod')


import           Entities.Fly
import           Entities.Player


import           Entities

import           Game.Levels                (initialLevel)



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

                  _world     :: World

                 }

makeLenses ''GameState


initialState :: GameState
initialState = GameState 0 (execState (do
                                      newPlayer  (100,100) gravity
                                      newFly  (10,0) (20,0)
                                      initialLevel

    )emptyWorld)
{-initialState = tate 0 viewPortInit (Player.newPlayer (-285,-55) gravity) initialLevel-}



updateDT :: Double -> (Bool, Bool, Bool, Bool) -> State GameState Double
updateDT acc input = do
                        replicateM_ (div' acc deltaTime) (update input)
                        return $ mod' acc deltaTime

update :: (Bool, Bool, Bool, Bool) -> State GameState ()
update input = do
           totalTime += deltaTime

           world %= execState (do
                                 processAccelerations deltaTimeF
                                 processSpeedLimits
                                 processVelocities deltaTimeF
                                 processWorldBoundaries
                              )

           return ()
           {-currPlayer <- use player-}
           {-player .= execState (Player.update input deltaTimeF) currPlayer-}

           {-let useInCollision block = case block ^. blockType of Box  -> True; SandTop -> True; _ -> False-}
           {-solidtiles <- gets $ \gamestate -> Vector.filter useInCollision (gamestate ^.  level.tiles)-}

           {-player %= execState (collision solidtiles (currPlayer ^. position.y))-}


           {-boundaries <- use $ level.levelBounds-}
           {-player %= wrapAroundBounds boundaries-}

           {--- FLY-}
           {-level.enemies %= Vector.map (execState (integrateAcceleration deltaTimeF >> integrateSpeed deltaTimeF))-}
           {-level.enemies %= Vector.map (wrapAroundBounds boundaries)-}


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

topOfSquare :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
topOfSquare (u, v) (sx, sy) (w, h) = u > sx-w2 && u < sx+w2 && v > sy+h4 && v < sy+h2
                where w2 = w/2
                      h2 = h/2
                      h4 = h/4





