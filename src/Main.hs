{-# LANGUAGE PackageImports #-}

import           Control.Monad.State.Strict
import           Data.Maybe                 (fromJust)
import           Graphics.Gloss.Rendering   as RS
import           "GLFW-b" Graphics.UI.GLFW           as GLFW
import           System.Exit                (exitSuccess)

import           Control.Concurrent         (threadDelay)
import           Game
import           GLFWUtils

import           Rendering

windowWidth, windowHeight :: Int
windowWidth  = 2*width  -- 1280
windowHeight = 2*height -- 960


main :: IO ()
main = do
    glossState <- initState
    textures   <- loadTextures
    withWindow windowWidth windowHeight width height "Game-Demo" $ \win -> do
          _ <- runStateT (loop win glossState textures 0) initialState
          exitSuccess

loop :: Window -> RS.State -> Textures -> Double-> StateT GameState IO ()
loop window glossState texture acc = do
            dt <- lift $  fromJust <$> getTime
            lift $ setTime 0

            {-lift $ threadDelay 20000-}

            (k, l, r, u, d) <- lift $ getInputs window

            gamestate <- get
            (acc', newgamestate) <- return $  runState (updateDT (dt+acc) (l,r,u,d)) gamestate
            put newgamestate


            lift $ renderFrame texture newgamestate window glossState
            unless k $ loop window glossState texture acc'



getInputs :: Window -> IO (Bool, Bool, Bool, Bool, Bool)
getInputs win = do
            threadDelay 20000
            pollEvents
            k <- keyIsPressed win Key'Escape
            l <- keyIsPressed win Key'Left
            r <- keyIsPressed win Key'Right
            u <- keyIsPressed win Key'Up
            d <- keyIsPressed win Key'Down
            return (k,l,r,u,d)
