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

type Vector = (Double,Double)
type Pos    = Vector
type Dir    = Vector
type Color  = Int

data Pen    = Pen Bool Color
data Turtle = T Pos Dir Pen

-- | Description of your type here...
--
--   You can use newtype instead of data if you wish.
newtype Program = P (Turtle -> Turtle) -- ?

-- * Constructors
forward  :: Double -> Program
right    :: Double -> Program
penup    :: Program
pendown  :: Program
color    :: Program
die      :: Program

-- * Combinators
(>*>)    :: Program -> Program -> Program
limited  :: Int -> Program -> Program

-- * Derived Combinators
idle :: Program -- could be implemented as "forward 0" for example
idle = forward 0

backward :: Double -> Program
backward = forward . negate

left :: Double -> Program
left = right . negate

forever  :: Program -> Program
forever p = p >*> forever p

times :: Int -> Program -> Program
times n = limited n . forever

lifespan :: Int -> Program -> Program
lifespan n p = limited n p >*> die

-- * Run function
runTextual :: Program -> IO ()
runTextual = undefined

-- * Temporary undefined implementation

forward  = undefined
right    = undefined
penup    = undefined
pendown  = undefined
color    = undefined
die      = undefined
limited  = undefined
lifespan = undefined
times    = undefined
(>*>)    = undefined


