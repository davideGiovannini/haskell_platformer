module Entities.Player (
    newPlayer
    )
where

import           Components.Acceleration
import           Components.Bounds
import           Components.JumpAbility
import           Components.Position
import           Components.Renderable
import           Components.Velocity

import           Entities

import           Resources

import           Control.Monad.State.Strict



-- TODO costante provvisoria
friction :: Float
friction = 0.7

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

    updatePosOf entity (uncurry Position pos)
    updateVelOf entity (Velocity 0 0)
    updateAccOf entity (uncurry Acceleration acc)

    updateBoundsOf entity (uncurry Bounds playerSize)

    updateJumpAbOf entity (JumpAbility False jumpSpeed framesRecharcheJump)

    updateRenderOf entity (RenderAnim 9 AlienBlueWalk)

{-anchorPoint :: Player -> (Float, Float)-}
{-anchorPoint player = (px, py-(player ^. bounds.height) /2)-}
        {-where (px, py) = player ^. position.xy-}


{-updateSpeed :: (Bool, Bool, Bool, Bool) -> State Player ()-}
{-updateSpeed (l, r, u, d) = do-}
    {-onground <- use onGround-}
    {-when (l && not r) ( velocity.dx -= speed >> velocity.dx %= max (-maxWalkSpeed))-}
    {-when (r && not l) ( velocity.dx += speed >> velocity.dx %= min maxWalkSpeed)-}
    {-when (u && not d)  jump-}
    {-when (d && not u && onground) (velocity.dy -= speed)-}
    {-return ()-}


{-integrateStep :: Float -> State Player ()-}
{-integrateStep dt = do-}
                    {-integrateAcceleration dt-}
                    {-velocity.dy %= max (-maxFallSpeed)-}


                    {-onFloor    <- use onGround-}
                    {-when onFloor (velocity.dx %=(*friction))-}

                    {-integrateSpeed dt-}





{-update :: (Bool, Bool, Bool, Bool) -> Float -> State Player ()-}
{-update input dt = do-}
                {-jumpTimer %= \t -> if t > 0 then t-1 else 0-}

                {-updateSpeed input-}
                {-integrateStep dt-}




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



