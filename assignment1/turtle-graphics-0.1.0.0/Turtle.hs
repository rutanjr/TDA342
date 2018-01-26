{- |
Module      : Turtle
Description : 
-}
module Turtle (
  -- * The turtle type(s)
  -- Non-exhaustive list of possible types: Turtle, Program, Action, Operation ...
  Program, Action
  -- * Primitive operations
  , forward
  , right
  , penup
  , pendown
  , color
  , (>*>)
  , die
  , limited
  -- * Derived operations
  , forever
  , times
  , idle
  , lifespan
  , backward
  , left

  -- * Run functions
  , runTextual

  ) where

type Vector = (Double,Double)
type Line   = (Pos,Pos,Maybe Color)
type Pos    = Vector
type Dir    = Vector
type Color  = Int --placeholder

data Pen    = Pen Bool Color
data Turtle = T Pos Dir Pen
data Action
  = Act Line   -- ^ The constructor 'Act' for an action resulting in a line
  | Msg String -- ^ Constructor for turtle actions not resulting in lines

-- | A program is a function which takes a Turtle and returns a list of actions
-- performed and maybe a turtle depending on its survival.
newtype Program = P (Turtle -> ([Action],Maybe Turtle)) -- ?

-- * Constructors
forward :: Double -> Program
right   :: Double -> Program
penup   :: Program
pendown :: Program
color   :: Program
die     :: Program

-- * Combinators
(>*>)   :: Program -> Program -> Program
limited :: Int -> Program -> Program

-- * Derived Combinators
idle :: Program
idle = right 0

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
forward = undefined
right   = undefined
penup   = undefined
pendown = undefined
color   = undefined
die     = undefined
limited = undefined
(>*>)   = undefined
