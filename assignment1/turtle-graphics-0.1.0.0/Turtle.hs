{- |
Module      : Turtle
Description : Interface for writing turtle programs and a textual function to tun them.

The turtle interface provides primiteve, as well as derived, operators for creating turtle programs. The programs can be run with a textual function to produce a sequential list of performed actions.

-}
module Turtle (
  -- * The turtle types
  Program, Action, Vector, Line, Pos, Dir
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
type Line   = (Pos,Pos,Color)
type Pos    = Vector
type Dir    = Vector
type Color  = Int -- placeholder

data Pen    = Pen Bool Color
data Turtle = T Pos Dir Pen
data Action
  = Act Line   -- ^ The constructor 'Act' for an action resulting in a line
  | Msg String -- ^ Constructor for turtle actions not resulting in lines

-- | A program is a function which takes a Turtle and returns a list of actions
-- performed and maybe a turtle, depending on its survival.
newtype Program = P (Turtle -> ([Action],Maybe Turtle))

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
