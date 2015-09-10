{-# LANGUAGE TemplateHaskell #-}
module Components.JumpAbility
    where

import           Control.Lens


data JumpAbility = JumpAbility {
                            _onGround           :: Bool,
                            _jumpForce          :: Float,
                            _jumpIncrSpeed      :: Float,
                            _jumpTimer          :: Int,
                            _framesJumpPower    :: Int,
                            _framesJumpRecharge :: Int
                          }deriving Show


makeLenses ''JumpAbility



