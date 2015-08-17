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

import           Control.Monad.Reader

windowWidth, windowHeight :: Int
windowWidth  = 2*width  -- 1280
windowHeight = 2*height -- 960


main :: IO ()
main = do
    glossState <- initState
    resources   <- loadResources
    withWindow windowWidth windowHeight width height "Game-Demo" $ \win -> do
          _ <- runStateT (loop win glossState resources 0) initialState
          exitSuccess

loop :: Window -> RS.State -> Resources -> Double-> StateT GameState IO ()
loop window glossState resources acc = do
            dt <- lift $  fromJust <$> getTime
            lift $ setTime 0

            {-lift $ threadDelay 20000-}

            (k, l, r, u, d) <- lift $ getInputs window

            gamestate <- get
            (acc', newgamestate) <- return $  runState (updateDT (dt+acc) (l,r,u,d)) gamestate
            put newgamestate


            lift $ runReaderT (renderFrame resources window glossState) newgamestate
            unless k $ loop window glossState resources acc'



getInputs :: Window -> IO (Bool, Bool, Bool, Bool, Bool)
getInputs win = do
            threadDelay 20000
            pollEvents
            k <- keyIsPressed win Key'Escape
            l <- keyIsPressed win Key'A
            r <- keyIsPressed win Key'D
            u <- (||) <$> keyIsPressed win Key'W <*> keyIsPressed win Key'Space
            d <- keyIsPressed win Key'S
            return (k,l,r,u,d)

