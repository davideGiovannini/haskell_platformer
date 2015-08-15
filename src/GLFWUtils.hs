{-# LANGUAGE PackageImports #-}
module GLFWUtils where
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

import           Control.Monad             (when)
import           "GLFW-b" Graphics.UI.GLFW          as GLFW

withWindow :: Int -> Int -> Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow windowWidth windowHeight internalWidth internalHeight title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        m <- GLFW.createWindow windowWidth windowHeight title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              GLFW.setWindowSizeCallback win (Just $ resize (fromIntegral internalWidth) (fromIntegral internalHeight))
              f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed win key = isPress `fmap` GLFW.getKey win key

isPress :: KeyState -> Bool
isPress KeyState'Pressed   = True
isPress KeyState'Repeating = True
isPress _ = False


resize ::Double -> Double -> Window -> Int -> Int -> IO()
resize inW inH _ w h = do
    let viewPortAR = if w <= h then fromIntegral w/inW else fromIntegral h/inH :: Double

    -- These are all analogous to the standard OpenGL functions
    GL.viewport $= (GL.Position 0 0, GL.Size (round $ inW*viewPortAR) (round $ inH * viewPortAR))
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.perspective 45 (fromIntegral w / fromIntegral h) 1 100

    GL.matrixMode $= GL.Modelview 0
