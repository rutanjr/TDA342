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
  , (<|>)
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

-- * Linear algebra and stuff
--------------------------------------------------------------------------------

-- | Vector addition
(+++) :: Vector -> Vector -> Vector
(x,y) +++ (u,v) = (x+u,y+v)

(***) :: Double -> Vector -> Vector
d *** (x,y) = (d*x,d*y)

-- | Rotate vector, basically matrix multiplication with 2d rotation matrix
rotate :: Double -> Vector -> Vector
rotate d (x,y) = (cos d * x + sin d * y, cos d * y - sin d * x)

-- * Turtle data type and functions
--------------------------------------------------------------------------------

-- | A program is a function which takes a Turtle and returns a list of actions
-- performed and maybe a turtle, depending on its survival.
newtype Program = P (Turtle -> Int -> ([(Int,Action)],Maybe Turtle))

-- * Constructors

-- | Moves turtle a distance 'd' in the direction it is facing 
forward :: Double -> Program
forward d = P $ \(T pos dir pen) n ->
  ([(n+1,Msg ("forward " ++ show d))], Just $ T (pos +++ (d *** dir)) dir pen)

-- | Rotates turtle 'd' radians
right :: Double -> Program
right d = P $ \(T pos dir pen) n ->
  ([(n+1,Msg ("turned " ++ show d ++ " radians"))],
   Just $ T pos (rotate d dir) pen)

penup   :: Program
pendown :: Program
color   :: Program
die     :: Program

-- * Combinators
(>*>)   :: Program -> Program -> Program

(<|>)   :: Program -> Program -> Program
(<|>) = undefined

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
penup   = undefined
pendown = undefined
color   = undefined
die     = undefined
limited = undefined
(>*>)   = undefined


-- * Attempt at monad implementation
--------------------------------------------------------------------------------

-- -- Functor
-- instance Functor E where
--    fmap f m = m >>= \a -> return (f a)

-- -- Applicative
-- instance Applicative E where
--    pure  = return
--    (<*>) a_f a_x = a_f >>= \f -> a_x >>= \x -> pure $ f x
