{- |
Module      : TurtleExtras
Description : Some extra features to the standard turtle, with some predefined
shapes and in different colors etc.
-}
module TurtleExtras where

import Turtle
import Graphics.HGL


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




