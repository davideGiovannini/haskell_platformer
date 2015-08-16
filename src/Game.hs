{-# LANGUAGE TemplateHaskell #-}
module Game (
        GameState,
        initialState,
        width,
        height,
        deltaTime,
        player,
        blocks,
        updateDT
        )
where

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Fixed                 (div', mod')

import qualified Game.Blocks                as Blocks
import qualified Game.Player                as Player


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
                  _player :: Player.Player,
                  _blocks :: [Blocks.Block]

                 } deriving Show

makeLenses ''GameState


initialState :: GameState
initialState = GameState  (Player.newPlayer (-285,-55) gravity) [
                                        Blocks.Box      (-285,5),
                                        Blocks.Box      (-285,-65),
                                        Blocks.SandCenter (-285,-205),
                                        Blocks.SandTop (-285,-135),
                                        Blocks.SandCenter (-215,-205),
                                        Blocks.SandTop (-215,-135),
                                        Blocks.SandTop (75,-205),
                                        Blocks.SandTop (145,-205),
                                        Blocks.SandTop (215,-205),
                                        Blocks.SandTop (285,-205),
                                        Blocks.Box      (285,-135)]




updateDT :: Double -> (Bool, Bool, Bool, Bool) -> State GameState Double
updateDT acc input = do
                        replicateM_ (div' acc deltaTime) (update input)
                        return $ mod' acc deltaTime

update :: (Bool, Bool, Bool, Bool) -> State GameState ()
update input = do
           currPlayer <- use player
           player .= execState (Player.update input deltaTimeF) currPlayer

           let useInCollision block = case block of Blocks.Box _ -> True; Blocks.SandTop _ -> True; _ -> False
           tiles <- state $ \x -> (x ^..  blocks.traversed.filtered useInCollision, x)

           player %= execState (collision tiles (currPlayer ^. Player.position._2))

           player %= execState wrapAroundBounds




wrapAroundBounds :: State Player.Player ()
wrapAroundBounds = do
               Player.position._1 %= \ x -> if x < -w2 then w2 else if x > w2 then -w2 else x
               Player.position._2 %= \ y -> if y < -h2 then h2 else if y > h2 then -h2 else y
          where
               (w2, h2) = (fromIntegral width /2, fromIntegral height /2)


collision :: [Blocks.Block] -> Float -> State Player.Player ()
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


