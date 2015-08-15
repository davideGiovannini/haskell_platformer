{-# LANGUAGE TemplateHaskell #-}
module Game (
        GameState,
        initialState,
        width,
        height,
        deltaTime,
        player,
        updateDT
        )
where

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Fixed                 (div', mod')

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
                  _player :: Player.Player


                 } deriving Show

makeLenses ''GameState


initialState :: GameState
initialState = GameState $ Player.Player (0,0) (0,0)







updateDT :: Double -> (Bool, Bool, Bool, Bool) -> State GameState Double
{-updateDT acc input = state $ \game ->-}
                                {-(mod' acc deltaTime, execState (replicateM (div' acc deltaTime) (update input) ) game)-}
updateDT acc input = do
                        replicateM_ (div' acc deltaTime) (update input)
                        return $ mod' acc deltaTime

update :: (Bool, Bool, Bool, Bool) -> State GameState ()
update input =
    player %= execState (Player.update input deltaTimeF)




