{-# LANGUAGE TemplateHaskell #-}
module Game.Entities
    (
        BasicEntity,
        CollisionEntity,
        PhysicalEntity,

        ComponentPosition(..),
        ComponentBounds(..),
        ComponentVelocity(..),
        ComponentAcceleration(..),

        position,
        bounds,
        velocity,
        acceleration,

        integrateSpeed,
        integrateAcceleration,

        x, y, xy,
        width, height, wh,
        dx, dy, dxy,
        ax, ay, axy
    )

where

import           Control.Arrow              ((&&&), (***))
import           Control.Lens

import           Control.Monad.State.Strict (State)

--- BASIC ENTITY with Position  --------------------

class BasicEntity a where
     position :: Lens' a ComponentPosition


data ComponentPosition = Position{
                                   _x :: Float,
                                   _y :: Float
                                 } deriving(Show, Eq)

xy :: Lens' ComponentPosition (Float, Float)
xy = lens (_x &&& _y ) (\pos (nx, ny) -> pos {_x = nx, _y = ny})




--- CollisionEntity   a BasicEntity with Bounds

class BasicEntity entity => CollisionEntity entity where
    bounds :: Lens' entity ComponentBounds

data ComponentBounds = Bounds {
                                _width  :: Float,
                                _height :: Float
                              }deriving(Show, Eq)


wh :: Lens' ComponentBounds (Float, Float)
wh = lens (_width &&& _height ) (\size (nw, nh) -> size {_width = nw, _height = nh})





---- PhysicalEntity   a CollisionEntity with Speed and Acceleration

class BasicEntity entity => PhysicalEntity entity where
    velocity     :: Lens' entity ComponentVelocity
    acceleration :: Lens' entity ComponentAcceleration


data ComponentVelocity = Velocity{
                                  _dx :: Float,
                                  _dy :: Float
                                 } deriving(Show, Eq)

dxy :: Lens' ComponentVelocity (Float, Float)
dxy = lens (_dx &&& _dy ) (\vel (ndx, ndy) -> vel {_dx = ndx, _dy = ndy})

data ComponentAcceleration = Acceleration{
                                  _ax :: Float,
                                  _ay :: Float
                                 } deriving(Show, Eq)

axy :: Lens' ComponentAcceleration (Float, Float)
axy = lens (_ax &&& _ay ) (\acc (nax, nay) -> acc {_ax = nax, _ay = nay})


makeLenses ''ComponentPosition
makeLenses ''ComponentBounds
makeLenses ''ComponentVelocity
makeLenses ''ComponentAcceleration



integrateAcceleration :: PhysicalEntity ent => Float -> State ent ()
integrateAcceleration dt = do
                        (ax', ay') <- use $ acceleration.axy
                        velocity.dxy %= ((+ (dt*ax'))  ***  (+ (dt*ay')))


integrateSpeed :: PhysicalEntity ent => Float -> State ent ()
integrateSpeed dt = do
                        (vx', vy') <- use $ velocity.dxy
                        position.xy %= ((+ (dt*vx'))  ***  (+ (dt*vy')))






