module Game.Viewport
   (
   )
where

{-import           Control.Lens-}

{-import           Game.Entities.Player         (Player, onGround)-}
{-import           Graphics.Gloss.Data.Vector   (magV, mulSV, normalizeV)-}

{-import           Graphics.Gloss.Data.ViewPort-}

{-import           Game.Entities                (dxy, position, velocity, xy)-}

{-followPlayer :: Float -> Player -> ViewPort -> ViewPort-}
{-followPlayer dt _player (ViewPort (x, y) r s) =-}
                            {-let (tx, ty) = mulSV (-1) (_player ^. position.xy)-}
                                {-(vx, vy) = ofMaxLenght 400 (mulSV 10 $ _player ^. velocity.dxy)-}
                                {-distanceV@(dx, dy) = (tx-x-vx, ty-70-y-vy) -- -70 to view more above the player-}
                                {-magDist = magV distanceV-}

                                {-onground = _player ^. onGround-}
                                {-(xx, yy) =  if magDist > 75 then-}
                                                {-if onground || magDist < 60 then-}
                                                    {-(x+(dx*dt), y+(dy*dt))-}
                                                {-else-}
                                                    {-(x+(dx*dt), y)-}
                                            {-else-}
                                                {-(x, y+(dy*dt))-}
                            {-in-}
                            {-ViewPort (xx, yy)  r s-}


{-ofMaxLenght :: Float -> (Float, Float) -> (Float, Float)-}
{-ofMaxLenght maxL vect = if magV vect < maxL then-}
                            {-vect-}
                        {-else-}
                           {-let norm = normalizeV vect-}
                           {-in-}
                           {-mulSV maxL norm-}

