module Entities.Player (
    newPlayer
    )
where

import           Components.Acceleration
import           Components.Bounds
import           Components.Input
import           Components.JumpAbility
import           Components.MaxSpeed
import           Components.Position
import           Components.Renderable
import           Components.Velocity
import Components.Collisions

import           Entities
import Systems

import           Resources

import           Control.Monad.State.Strict

import           Control.Lens               ((&), (+~), (-~))




maxFallSpeed :: Float
maxFallSpeed = 1350



playerSize :: (Float, Float)
playerSize  = (66, 92)

speed :: Float
speed = 100

maxWalkSpeed :: Float
maxWalkSpeed = 500

jumpSpeed :: Float
jumpSpeed = 800

jumpIncrSpeed :: Float
jumpIncrSpeed = 30


-- TODO better names and maybe refactoring
framesJump :: Int
framesJump = 30

framesRecharcheJump :: Int
framesRecharcheJump = 5


newPlayer :: (Float, Float) -> (Float, Float) -> State World ()
newPlayer pos acc = do
    entity <- newEntity

    updateEntity $ entity & position       -| uncurry Position pos
                          & velocity       -| Velocity 0 0
                          & acceleration   -| uncurry Acceleration acc

                          & bounds         -| uncurry Bounds playerSize

                          & jumpAbility    -| JumpAbility False jumpSpeed framesRecharcheJump

                          & maxSpeed       -| MaxSpeed maxWalkSpeed maxFallSpeed

                          & renderable     -| RenderAnim 9 AlienBlueWalk

                          & inputProcessor -| playerInputProcessor
                          & collider       -| Collider



playerInputProcessor :: InputProcessor
playerInputProcessor entity input =
                    when (entity `has` velocity ) (do
                       let vel = velocity `from` entity
                           horizontalSpeedFun
                                      | r && not l = velocity -| (vel & dx +~ speed)
                                      | l && not r = velocity -| (vel & dx -~ speed)
                                      | otherwise  = id
                           verticalSpeedFun = if entity `has` jumpAbility then
                                                 let jump = jumpAbility `from` entity in
                                                     if j && _onGround jump then
                                                        (velocity -| (vel & dy +~ _jumpForce jump)).(jumpAbility -| (jump {_onGround = False}))
                                                     else id
                                              else
                                                  id
                       updateEntity $ entity & horizontalSpeedFun & verticalSpeedFun
                       )
                    where l = _left input
                          r = _right input
                          j = _up input





{-landOn :: Float -> State Player ()-}
{-landOn y' = do-}
    {-onground <- use onGround-}
    {-unless onground (do-}
            {-onGround    .= True-}
            {-jumpTimer   .= framesRecharcheJump)-}
    {-velocity.dy .= 0-}
    {-position.y .= y'-}



{-jump :: State Player ()-}
{-jump = do-}
    {-onground <- use onGround-}
    {-frames   <- use jumpTimer-}

    {-if onground && frames == 0 then do-}
                            {-velocity.dy += jumpSpeed-}
                            {-onGround .= False-}
                            {-jumpTimer .= framesJump-}
    {-else-}
        {-when (not onground && frames >0) (velocity.dy += jumpIncrSpeed)-}



