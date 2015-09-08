module Game.Viewport
   (
       followEntity
   )
where

import           Control.Lens hiding (has, from)

import           Graphics.Gloss.Data.Vector   (magV, mulSV, normalizeV)

import           Graphics.Gloss.Data.ViewPort

import Entities
import Systems
import Components.Position
import Components.Velocity
import Components.JumpAbility


followEntity :: Float -> Entity -> ViewPort -> ViewPort
followEntity dt entity viewP@(ViewPort (x', y') r s) =
                            if entity `has` position then
                                let (tx, ty) = mulSV (-1) ((position `from` entity) ^. xy)
                                    (vx, vy) = ofMaxLenght 400 (mulSV 10 $ speed ^. dxy)
                                    distanceV@(d_x, d_y) = (tx-x'-vx, ty-70-y'-vy) -- -70 to view more above the player
                                    magDist = magV distanceV

                                    (xx, yy) =  if magDist > 75 then
                                                    if onground || magDist < 60 then
                                                        (x'+(d_x*dt), y'+(d_y*dt))
                                                    else
                                                        (x'+(d_x*dt), y')
                                                else
                                                    (x', y'+(d_y*dt))
                                in
                                ViewPort (xx, yy)  r s
                            else
                               viewP
                            where speed = if entity `has` velocity then
                                             velocity `from` entity
                                          else
                                             Velocity 0 0
                                  onground = not (entity `has` jumpAbility) || _onGround (jumpAbility `from` entity)


ofMaxLenght :: Float -> (Float, Float) -> (Float, Float)
ofMaxLenght maxL vect = if magV vect < maxL then
                            vect
                        else
                           let norm = normalizeV vect
                           in
                           mulSV maxL norm

