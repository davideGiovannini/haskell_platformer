module Components.Input
    where



data Input = Input {
                     _left  :: Bool,
                     _right :: Bool,
                     _up    :: Bool,
                     _down  :: Bool
                   } deriving (Show, Eq)



