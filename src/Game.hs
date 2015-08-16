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


data GameState = GameState {
                  _player :: Player.Player,
                  _blocks :: [Blocks.Block]

                 } deriving Show

makeLenses ''GameState


initialState :: GameState
initialState = GameState  (Player.newPlayer (-285,-55) (0, -4800)) ([
                                        Blocks.SandCenter (-285,-205),
                                        Blocks.SandTop (-285,-135),
                                        Blocks.SandCenter (-215,-205),
                                        Blocks.SandTop (-215,-135),
                                        Blocks.SandTop (145,-205),
                                        Blocks.SandTop (215,-205),
                                        Blocks.SandTop (285,-205),
                                        Blocks.Box      (285,-135)]

                                        ++
--temp to close hole
                                        [
                                        Blocks.SandTop (-285,-275),
                                        Blocks.SandTop (-215,-275),
                                        Blocks.SandTop (-145,-275),
                                        Blocks.SandTop (-75,-275),
                                        Blocks.SandTop (-5,-275),
                                        Blocks.SandTop (65,-275),
                                        Blocks.SandTop (135,-275),
                                        Blocks.SandTop (205,-275),
                                        Blocks.SandTop (275,-275)

                                        ])







updateDT :: Double -> (Bool, Bool, Bool, Bool) -> State GameState Double
updateDT acc input = do
                        replicateM_ (div' acc deltaTime) (update input)
                        return $ mod' acc deltaTime

update :: (Bool, Bool, Bool, Bool) -> State GameState ()
update input = do
           currPlayer <- use player
           let player' = execState (Player.update input deltaTimeF) currPlayer
           if inBounds player' then
            player .= player'
           else
             do
               player.Player.position .=  (currPlayer ^. Player.position)
               player.Player.velocity .= (0, 0)

           let useInCollision block = case block of Blocks.Box _ -> True; Blocks.SandTop _ -> True; _ -> False
           tiles <- state $ \x -> (x ^..  blocks.traversed.filtered useInCollision, x)

           player %= collision tiles (currPlayer ^. Player.position._2)

           clampBounds


inBounds :: Player.Player -> Bool
inBounds p =
    x> -w2 && x < w2 && y > -h2 && y< h2

    where (x, y)   = p ^. Player.position
          (w2, h2) = (fromIntegral width /2, fromIntegral height /2)


clampBounds :: State GameState ()
clampBounds = do
               player.Player.position._1 %= \ x -> max (-w2) (min w2 x)
               player.Player.position._2 %= \ y -> max (-h2) (min h2 y)
          where
               (w2, h2) = (fromIntegral width /2, fromIntegral height /2)


collision :: [Blocks.Block] -> Float -> Player.Player -> Player.Player
collision tiles oldY player' = if falling && any shouldStopFall tiles then
                            ((player' & Player.velocity._2 .~ 0) & Player.onGround .~ True) & Player.position._2 .~ oldY

                          else
                          player'
    where falling = player' ^. Player.velocity._2 <0
          playerAnchor = Player.anchorPoint player'
          shouldStopFall block  = topOfSquare playerAnchor (block ^. Blocks.pos) Blocks.blockSize






insideSquare :: (Float, Float) -> (Float, Float) -> Float -> Bool
insideSquare (x, y) (sx, sy) w = x > sx-w2 && x < sx+w2 && y > sy-w2 && y < sy+w2
                where w2 = w/2

topOfSquare :: (Float, Float) -> (Float, Float) -> Float -> Bool
topOfSquare (x, y) (sx, sy) w = x > sx-w2 && x < sx+w2 && y > sy+w4 && y < sy+w2
                where w2 = w/2
                      w4 = w/4


