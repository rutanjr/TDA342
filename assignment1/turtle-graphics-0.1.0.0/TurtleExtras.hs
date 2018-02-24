{- |
Module      : TurtleExtras
Description : Some extra features to the standard turtle, with some predefined
shapes and in different colors etc.
-}
module TurtleExtras where

import Turtle
import Graphics.HGL hiding (line)

-- * Functions drawing shapes in specified colors
--------------------------------------------------------------------------------

-- | Draws a square with each side of specified length
-- in the specified color.
-- After it's done turns the turtle back to the starting state 
-- (facing the same direction and in the same position)
-- and leaves the pen down, with the specified color set.
drawSquare :: Double -> Color -> Program
drawSquare n c = pendown >*> color c 
    >*> times 4 (forward n >*> right (0.5 * pi))

-- | Draws a triangle with each side of specified length
-- in the specified color.
drawTriangle :: Double -> Color -> Program
drawTriangle n c = pendown >*> color c 
    >*> times 3 (forward n >*> right ((2/3) * pi))

-- | Draws a shape with the specified amount of edges, each of the specified 
-- length, in the specified color. 
drawShape :: Int -> Double -> Color -> Program
drawShape n d c = pendown >*> color c 
    >*> times n (forward d >*> right (2/fromIntegral n * pi))

-- | Draws a shape with specified amount of edges, each of length specified by
-- user, in color Magenta.
drawMagentaShape :: Int -> Double -> Program
drawMagentaShape n d = pendown >*> color Magenta
    >*> times n (forward d >*> right (2/fromIntegral n * pi))

-- * Functions drawing shapes in rainbow colors!
--------------------------------------------------------------------------------

-- | Draws a square with each side of specified length
-- in rainbow colored sides (first blue, then green, yellow and last red).
drawRainbowSquare :: Double -> Program
drawRainbowSquare n = pendown 
    >*> color Blue >*> forward n >*> right (0.5 * pi)
    >*> color Green >*> forward n >*> right (0.5 * pi)
    >*> color Yellow >*> forward n >*> right (0.5 * pi)
    >*> color Red >*> forward n >*> right (0.5 * pi)

-- * Higher order functions
--------------------------------------------------------------------------------

-- | Given a function 'ta' and a function 'f' create an infite sequence of
-- turtle programs where  the function 'f' is applied the argument of 'ta' each
-- recursive call.
transform :: (a -> Program) -> (a -> a) -> a -> Program
transform tp f a = (tp a) >*> transform tp f (f a)

-- | Recreation of spiral program using the transform program
spiral :: Double -> Double -> Program
spiral s d = transform (\s -> forward s >*> right d) (+2) s

-- | Creates tree using transform
tree :: Double -> Double -> Program
tree s d = transform branch (*0.8) s
  where branch s = forward s >*> (left d <|> right d)

-- | The same as transform except that that it uses 2 variables that change
-- each iteration.
transform2 :: (a -> b -> Program) -> (a -> a) -> (b -> b) -> a -> b -> Program
transform2 tp f g a b = transform (uncurry tp) h (a,b)
  where h = \(a,b) -> (f a, g b)

-- | Draws a tree with branches that shift in size and color as it gets deeper.
colorTree :: Double -> Double -> Program
colorTree s d = transform2 branch (*0.8) succ s Blue
  where branch s c = color c >*> forward s >*> (left d <|> right d)
