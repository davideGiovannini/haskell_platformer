{-# LANGUAGE TemplateHaskell #-}
module Game.Player (
    Player(..),
    update,
    playerSize,
    position,
    velocity
    )
where

import           Control.Lens
import           Control.Monad.State.Strict
import           Graphics.Gloss             (Point)


playerSize :: Float
playerSize = 20

speed :: Float
speed =  5* 100

data Player = Player {
                      _position :: Point,
                      _velocity :: Point
                     } deriving Show

makeLenses ''Player


updateSpeed :: (Bool, Bool, Bool, Bool) -> State Player ()
updateSpeed (True,  _,     _,     _)     = velocity .= (-speed, 0)
updateSpeed (_,     True,  _,     _)     = velocity .= (speed,0)
updateSpeed (_,     _,     True,  _)     = velocity .= (0,speed)
updateSpeed (_,     _,     _,     True)  = velocity .= (0,-speed)
updateSpeed (False, False, False, False) = velocity .= (0,0)



integrateSpeed :: Float -> State Player ()
integrateSpeed dt = state $ \player@(Player (x,y) (dx,dy)) -> ((), player{_position =  (x+dx*dt, y+dy*dt)})



update :: (Bool, Bool, Bool, Bool) -> Float -> State Player ()
update input dt = do
                updateSpeed input
                integrateSpeed dt
