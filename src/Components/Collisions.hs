module Components.Collisions
where




data Collider = Collider

              deriving Eq




data Collidable = Platform
                | Tile
                deriving Eq



newtype Friction = Friction { getFriction :: Float } deriving (Eq, Show)

