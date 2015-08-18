module Game.Viewport where

import           Control.Lens

import           Game.Player                  (Player)
import qualified Game.Player                  as Player
import           Graphics.Gloss.Data.Vector   (magV, mulSV, normalizeV)

import           Graphics.Gloss.Data.ViewPort

followPlayer :: Float -> Player -> ViewPort -> ViewPort
followPlayer dt _player (ViewPort (x, y) r s) =
                            let (tx, ty) = mulSV (-1) (_player ^. Player.position)
                                (vx, vy) = ofMaxLenght 400 (mulSV 10 $ _player ^. Player.velocity)
                                distanceV@(dx, dy) = (tx-x-vx, ty-70-y-vy) -- -70 to view more above the player
                                magDist = magV distanceV

                                onground = _player ^. Player.onGround
                                (xx, yy) =  if magDist > 75 then
                                                if onground || magDist < 60 then
                                                    (x+(dx*dt), y+(dy*dt))
                                                else
                                                    (x+(dx*dt), y)
                                            else
                                                (x, y+(dy*dt))
                            in
                            ViewPort (xx, yy)  r s


ofMaxLenght :: Float -> (Float, Float) -> (Float, Float)
ofMaxLenght maxL vect = if magV vect < maxL then
                            vect
                        else
                           let norm = normalizeV vect
                           in
                           mulSV maxL norm

