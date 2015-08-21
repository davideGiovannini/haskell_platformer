{-# LANGUAGE TemplateHaskell #-}
module Components.JumpAbility
    where

import           Control.Lens


data JumpAbility = JumpAbility {
                            _onGround  :: Bool,
                            _jumpForce :: Float,
                            _jumpTimer :: Int
                          }


makeLenses ''JumpAbility


