{- |
Module      : TestPrograms
Description : Some small tests to have some templates to run in the Turtle
 language.
-}
module TestPrograms where

import Turtle
import Graphics.HGL


-- * Test programs, pick one and enjoy. None is to complicated.
--------------------------------------------------------------------------------


-- | Draws a ling
prog_1 = forward 2

-- | Turn action, not actually turning, and thn turning right 1 rad.
prog_2 = right 0 >*> right 1
-- | Small test program, som turning, lifting pen, moves then puts pen down 
-- and draws a short line.
prog_3 = right (pi/2) >*> penup >*> forward 1 >*> pendown >*> forward 1

-- | Testing that our turtles death actually kills the program.
progNotForever = idle >*> die >*> forever idle
 
-- | Turns then draws a basic line of length 100
progLines d = right d >*> forward 50 >*> forward 50

-- | Draws a simple cross, parallel.
progCross = progLines 0 <|> progLines (pi/2) <|> progLines pi <|> progLines (-pi/2)

-- | Draws a square.
progSquare = times 4 (forward 100 >*> right (pi/2))

-- | Draws a red square.
progRedSquare = color Red >*> times 4 (forward 100 >*> right (pi/2))

-- | Runs forever. Be careful
progForever = forever idle

