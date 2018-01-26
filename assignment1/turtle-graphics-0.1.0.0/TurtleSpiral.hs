-- | 
module TurtleSpiral where

import Turtle1

-- | A finite spiral turning d radians each step and stopping at 100 steps
spiralFin :: Double -> Double -> Program
spiralFin s d | s > 100   = die
              | otherwise = forward s >*> right d >*> spiralFin (s + 2) d

-- | An infinite spiral turning d radians each step
spiralInf :: Double -> Double -> Program
spiralInf s d = forward s >*> right d >*> spiralInf (s + 2) d

-- | An alternate definition of spiralFin using spiralInf
spiralFin' :: Double -> Double -> Program
spiralFin' s d = limited (102 - s `div` 2) $ spiralInf s d

-- | An infinite spiral program starting with a finite spiral and then an
-- infinite spiral continuing where the finite ends
spiralFinInf :: Double -> Double -> Program
spiralFinInf s d = spiralFin s d >*> spiralInf 102 d
