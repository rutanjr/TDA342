-- | A graphical run function for the turtle DSL
module TurtleGraphics (runGraphical) where

import Turtle
import TurtleSpiral

import Graphics.HGL
import Prelude hiding (lines)


-- | Draw the resulting lines of running a turtle program. The initial 'Turtle'
-- will allways start in the center of the windows.
runGraphical :: Program -> IO ()
runGraphical p = runGraphics $ do
  window <- openWindowEx "Turtle!" Nothing (w,h) DoubleBuffered (Just 100)
  drawInWindow window (polygon [(0,0),(0,h),(w,h),(w,0)])
  let ls = runLines p
  onTick window ls
  getKey window >> return ()
  where
    h = 600
    w = 600    

-- | One graphical tick drawing one line from a list of lines
onTick :: Window -> [Line] -> IO ()
onTick w [] = return ()
onTick w (l:ls) = do
  getWindowTick w
  (_,(width,height)) <- getWindowRect w
  let p1 = fromVector (from l) width height
      p2 = fromVector (to l)   width height        
  drawInWindow w $ withColor Black $ line p1 p2
  onTick w ls

fromVector :: Vector -> Int -> Int -> (Int,Int)
fromVector (x,y) w h = (div w 2 + floor x, div h 2 - floor y)

-- * test programs
--------------------------------------------------------------------------------

prog_lines d = right d >*> forward 50 >*> forward 50
prog_cross = prog_lines 0 <|> prog_lines (pi/2) <|> prog_lines (pi) <|> prog_lines (-pi/2)
prog_square = times 10 (forward 100 >*> right (pi/2))
