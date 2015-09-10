module Entities.Player (
    newPlayer
    )
where

import           Components.Acceleration
import           Components.Bounds
import           Components.Collisions
import           Components.Direction
import           Components.Input
import           Components.JumpAbility(JumpAbility(JumpAbility, _onGround))
import           Components.MaxSpeed
import           Components.Position
import           Components.Renderable
import           Components.Velocity

import           Entities
import           Systems

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

framesJumpExtra :: Int
framesJumpExtra = 30

framesRecharcheJump :: Int
framesRecharcheJump = 5


newPlayer :: (Float, Float) -> (Float, Float) -> State World Entity
newPlayer pos acc =
        getNewEntity $  position       <== uncurry Position pos
                    |.| velocity       <== Velocity 0 0
                    |.| acceleration   <== uncurry Acceleration acc

                    |.| bounds         <== uncurry Bounds playerSize

                    |.| jumpAbility    <== JumpAbility False jumpSpeed jumpIncrSpeed 0 framesJumpExtra framesRecharcheJump

                    |.| maxSpeed       <== MaxSpeed maxWalkSpeed maxFallSpeed

                    |.| renderable     <== playerStateRenderer

                    |.| inputProcessor <== playerInputProcessor
                    |.| collider       <== Collider
                    |.| direction      <== SpeedDirection



playerInputProcessor :: InputProcessor
playerInputProcessor entity input =
                    when (entity `has` velocity ) (do
                       let vel = velocity `from` entity
                           horizontalSpeedFun
                                      | r && not l = velocity <== (vel & dx +~ speed)
                                      | l && not r = velocity <== (vel & dx -~ speed)
                                      | otherwise  = id
                           verticalSpeedFun = if j then
                                                     jump
                                              else id

                       updateEntity $ entity & horizontalSpeedFun & verticalSpeedFun
                       )
                    where l = _left input
                          r = _right input
                          j = _up input


playerStateRenderer :: Entity -> Renderable
playerStateRenderer player
                | not onFloor  = RenderTexture AlienBlueJump
                | abs dx_ > 1  = RenderAnim 9  AlienBlueWalk
                | otherwise    = RenderTexture AlienBlue

                where (Velocity dx_ _) = if player `has` velocity then velocity `from` player else Velocity 0 0
                      onFloor = not (player `has` jumpAbility) || _onGround (jumpAbility `from` player)



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



