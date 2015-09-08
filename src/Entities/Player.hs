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

import           Control.Lens               ((&), (+~), (-~), (.~))




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

    updateEntity ( entity & position .~ Just(uncurry Position pos)
                          & velocity .~ Just(Velocity 0 0)
                          & acceleration .~ Just(uncurry Acceleration acc)

                          & bounds .~ Just(uncurry Bounds playerSize)

                          & jumpAbility .~ Just(JumpAbility False jumpSpeed framesRecharcheJump)

                          & maxSpeed .~ Just(MaxSpeed maxWalkSpeed maxFallSpeed)

                          & renderable .~ Just(RenderAnim 9 AlienBlueWalk)

                          & inputProcessor .~ Just playerInputProcessor
                          & collider .~ Just Collider
                 )



playerInputProcessor :: InputProcessor
playerInputProcessor entity input =
                    when (entity `has` velocity ) (do
                       let vel = velocity `from` entity
                       if r && not l then
                           update entity velocity (vel & dx +~ speed)
                       else
                           when (l && not r)
                                    (update entity velocity (vel & dx -~ speed))
                       when (entity `has` jumpAbility)
                             (do
                                let jump = jumpAbility `from` entity
                                when (j && _onGround jump) (update entity velocity (vel & dy +~ _jumpForce jump))

                             )
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



