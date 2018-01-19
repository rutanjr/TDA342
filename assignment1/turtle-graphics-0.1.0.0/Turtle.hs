-- | proper module documentation here
module Turtle (
  -- * The turtle type(s)
  -- Non-exhaustive list of possible types: Turtle, Program, Action, Operation ...
  Program

  -- * Primitive operations
  -- , forward
  -- , (>*>)
  -- , ...

  -- * Derived operations
  -- ...

  -- * Run functions
  -- runTextual :: Program -> IO ()
  -- ...

  ) where

-- | Description of your type here...
--
--   You can use newtype instead of data if you wish.
data Program =
  P {
    drawing :: Bool,
    moves :: [Move]
  }

data Move = T Double | M Double
{-
drawingturtle ::    Program
movingnotdrawing :: Program
resting ::          Program
dead ::             Program
-}
die :: Program -> Program
die _ = P {false, []}

idle :: Program -> Program
idle p = resting >*> p

penup :: Program -> Program

pendown :: Program -> Program

forward :: Double -> Program

backward :: Double -> Program

right :: Double -> Program

left :: Double -> Program 

times :: Int -> Program

color :: Program -> Program

limited :: Program -> Program

lifespan :: Program -> Program

forever :: Program -> Program

_>*>_ :: Program -> Program -> Program -- take two programs or take some kind of "action"?