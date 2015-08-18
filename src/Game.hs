{-# LANGUAGE TemplateHaskell #-}
module Game (
        GameState,
        initialState,
        width,
        height,
        deltaTime,
        player,
        level,
        viewport,
        totalTime,
        updateDT
        )
where

import           Control.Lens                 hiding (Level)
import           Control.Monad.State.Strict
import           Data.Fixed                   (div', mod')

import           Data.Vector                  (Vector)
import qualified Data.Vector                  as Vector (filter)

import           Game.Blocks                  (Block)
import qualified Game.Blocks                  as Blocks
import           Game.Levels                  (Level (getTiles), initialLevel)
import           Game.Player                  (Player)
import qualified Game.Player                  as Player

import           Graphics.Gloss.Data.ViewPort (ViewPort, viewPortInit)


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
                  _viewport  :: ViewPort,
                  _player    :: Player,
                  _level     :: Level

                 }

makeLenses ''GameState


initialState :: GameState
initialState = GameState 0 viewPortInit (Player.newPlayer (-285,-55) gravity) initialLevel



updateDT :: Double -> (Bool, Bool, Bool, Bool) -> State GameState Double
updateDT acc input = do
                        replicateM_ (div' acc deltaTime) (update input)
                        return $ mod' acc deltaTime

update :: (Bool, Bool, Bool, Bool) -> State GameState ()
update input = do
           totalTime += deltaTime

           currPlayer <- use player
           player .= execState (Player.update input deltaTimeF) currPlayer

           let useInCollision block = case block of Blocks.Box _ -> True; Blocks.SandTop _ -> True; _ -> False
           tiles <- gets $ \gamestate -> Vector.filter useInCollision (getTiles $ gamestate ^.  level)

           player %= execState (collision tiles (currPlayer ^. Player.position._2))

           player %= execState wrapAroundBounds




wrapAroundBounds :: State Player ()
wrapAroundBounds = do
               Player.position._1 %= \ x -> if x < -w2 then w2 else if x > w2 then -w2 else x
               Player.position._2 %= \ y -> if y < -h2 then h2 else if y > h2 then -h2 else y
          where
               (w2, h2) = (fromIntegral width /2, fromIntegral height /2)


collision :: Vector Block -> Float -> State Player ()
collision tiles oldY = do
        fallingSpeed <-  use $ Player.velocity._2
        anchorPoint  <- Player.anchorPoint <$> get

        if fallingSpeed < 0  && any (shouldStopFall anchorPoint) tiles then
            Player.landOn oldY
        else
            Player.onGround    .= False
    where
          shouldStopFall anchor block  = topOfSquare anchor (block ^. Blocks.pos) Blocks.blockSize




{-insideSquare :: (Float, Float) -> (Float, Float) -> Float -> Bool-}
{-insideSquare (x, y) (sx, sy) w = x > sx-w2 && x < sx+w2 && y > sy-w2 && y < sy+w2-}
                {-where w2 = w/2-}

topOfSquare :: (Float, Float) -> (Float, Float) -> Float -> Bool
topOfSquare (x, y) (sx, sy) w = x > sx-w2 && x < sx+w2 && y > sy+w4 && y < sy+w2
                where w2 = w/2
                      w4 = w/4


