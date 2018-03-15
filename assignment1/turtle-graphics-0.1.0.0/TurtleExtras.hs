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
loop :: (a -> Program) -> (a -> a) -> a -> Program
loop tp f a = (tp a) >*> loop tp f (f a)

-- | Recreation of spiral program using the loop program
spiral :: Double -> Double -> Program
spiral s d = loop (\s -> forward s >*> right d) (+2) s

-- | Creates tree using loop
tree :: Double -> Double -> Program
tree s d = loop branch (*0.8) s
  where branch s = forward s >*> (left d <|> right d)

-- | The same as loop except that that it uses 2 variables that change
-- each iteration.
loop2 :: (a -> b -> Program) -> (a -> a) -> (b -> b) -> a -> b -> Program
loop2 tp f g a b = loop (uncurry tp) h (a,b)
  where h = \(a,b) -> (f a, g b)

-- | Draws a tree with branches that shift in size and color as it gets deeper.
colorTree :: Double -> Double -> Program
colorTree s d = loop2 branch (*0.8) succ s Blue
  where branch s c = color c >*> forward s >*> (left d <|> right d)

-- * Turtle transformations
--------------------------------------------------------------------------------

-- | A transformation of all drawn lines and turtles. Only works propely for
-- linear transformations.
transform :: (Pos -> Pos) -> Program -> Program
transform f (P p) = P $ \t n ->
  let (as,ts) = p t n
  in (map (transAction f) as, map (transTurtle f) ts) 
  where
    transTurtle :: (Pos -> Pos) -> Turtle -> Turtle
    transTurtle f t = t { pos = f (pos t)
                        , dir = transDir f (pos t) (dir t) }

    transDir :: (Pos -> Pos) -> Pos -> Pos -> Pos
    transDir f p d = let p' = f p +++ f (p +++ d)
                     in 1 / (norm p') |* p'
                        
    transAction :: (Pos -> Pos) -> Action -> Action
    transAction f a = a { turtle = transTurtle f (turtle a)
                        , operation = transOp f (operation a)}

    transOp :: (Pos -> Pos) -> Operation -> Operation
    transOp f (Op l) = Op $ l { from = f (from l)
                              , to   = f (to l)}
    transOp _ op = op

-- | Rotates a turtle program 'd' radians around the origin.
rotate :: Double -> Program -> Program
rotate d p = transform f p
  where f (x,y) = (cos d * x + sin d * y, cos d * y - sin d * x)

-- | Scales a program with a factor s
scale :: Double -> Program -> Program
scale s p = transform (s |*) p

-- | Translates a program along the vector p
translate :: Pos -> Program -> Program
translate pos p =  transform (pos+++) p

-- * Some vector operations
--------------------------------------------------------------------------------

infixl 6 |*
infixl 5 +++

-- | Length of vector
norm :: Pos -> Double
norm (x,y) = sqrt (x^2 + y^2)

-- | Multiply scalar with vector            
(|*) :: Double -> Pos -> Pos
d |* (x,y) = (d*x,d*y)

-- | Add two vectors
(+++) :: Pos -> Pos -> Pos
(x,y) +++ (u,v) = (x+u,y+v)

