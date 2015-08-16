{-# LANGUAGE TemplateHaskell #-}
module Game.Player (
    newPlayer,
    Player,
    update,
    playerBounds,
    position,
    velocity,
    onGround,
    anchorPoint
    )
where

import           Control.Lens
import           Control.Monad.State.Strict
import           Graphics.Gloss             (Point)

-- TODO costante provvisoria
friction :: Float
friction = 0.7

maxFallSpeed :: Float
maxFallSpeed = 1350



playerBounds :: (Float, Float)
playerBounds  = (66, 92)

speed :: Float
speed =  100

maxWalkSpeed :: Float
maxWalkSpeed = 500

jumpSpeed :: Float
jumpSpeed = 1250

data Player = Player {
                      _position     :: Point,
                      _velocity     :: Point,
                      _acceleration :: Point,
                      _onGround     :: Bool
                     } deriving Show

makeLenses ''Player


newPlayer :: (Float, Float) -> (Float, Float) -> Player
newPlayer pos acc = Player pos (0,0) acc False


anchorPoint :: Player -> (Float, Float)
anchorPoint player = (x, y-(playerBounds ^. _2) /2)
        where (x, y) = player ^. position


updateSpeed :: (Bool, Bool, Bool, Bool) -> State Player ()
updateSpeed (l, r, u, d) = do
    canJump <- use onGround
    when (l && not r) ( velocity._1 -= speed >> velocity._1 %= max (-maxWalkSpeed))
    when (r && not l) ( velocity._1 += speed >> velocity._1 %= min maxWalkSpeed)
    when (u && not d && canJump ) (velocity._2 += jumpSpeed >> onGround .= False)
    when (d && not u && canJump) (velocity._2 -= speed)
    return ()


integrateSpeed :: Float -> State Player ()
integrateSpeed dt = do
                    onFloor <- use onGround
                    (ax, ay) <- use acceleration
                    velocity %= (\(vx,vy) -> (vx+ax*dt, max (vy+ay*dt) (-maxFallSpeed)))
                    when onFloor (velocity._1 %=(*friction))
                    (vx, vy) <- use velocity
                    position %= (\(x,y) -> (x+vx*dt, y+vy*dt))



update :: (Bool, Bool, Bool, Bool) -> Float -> State Player ()
update input dt = do
                updateSpeed input
                integrateSpeed dt



