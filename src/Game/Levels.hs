module Game.Levels
   (
       Level(getTiles),
       initialLevel
   )
where




import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Game.Blocks

newtype Level = Level {getTiles :: Vector Block }



initialLevel :: Level
initialLevel = Level $ Vector.fromList [
                                        Box      (-285,5),
                                        Box      (-285,-65),
                                        SandCenter (-285,-205),
                                        SandTop (-285,-135),
                                        SandCenter (-215,-205),
                                        SandTop (-215,-135),
                                        SandTop (75,-205),
                                        SandTop (145,-205),
                                        SandTop (215,-205),
                                        SandTop (285,-205),
                                        Box      (285,-135)
                                       ]

