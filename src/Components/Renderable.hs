module Components.Renderable where

import           Resources (Animation, Texture)

data Renderable = RenderTexture Texture
                     | RenderAnim Int Animation



