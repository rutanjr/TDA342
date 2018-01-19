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

data Program = Prog (Turtle -> ([Turtle], Maybe Turtle))
data Turtle = Tur Vector Vector Pen
data Vector = Vec {x, y :: Double}
data Pen = Pen {drawing :: Bool, col :: Int}
-- | Description of your type here...
--
--   You can urt newtype instead of data if you wish.

-- Constructors --

right :: Double -> Program
right d = Prog $ \(Tur pos dir pen) -> ([], Just (Tur pos (rotateRight d dir) pen))

forward :: Double -> Program
forward d = Prog $ \(Tur pos dir pen) -> ([], Just (Tur (pos +++ dir *** d) dir pen))

penup :: Program
penup = Prog $ penstat
  where penstat (Tur pos dir pen) = ([], Just turk)
          where turk = (Tur pos dir pen{drawing = False})

pendown :: Program 
pendown = Prog $ penstat
  where penstat (Tur pos dir pen) = ([], Just turk)
          where turk = (Tur pos dir pen{drawing = True}) 

color :: Int -> Program
color i = Prog $ newCol
  where newCol (Tur pos dir pen) = ([], Just turk)
          where turk = (Tur pos dir pen{col = i}) 

die :: Program
die = Prog $ \t -> ([], Nothing)


-- Combinators --

(>*>) :: Program -> Program -> Program
Prog p >*> Prog p' = Prog $ \t -> 
    case p t of
      (ts, Just t') -> let (ts', t'') = p' t'
                        in (ts ++ t': ts', t'')
      rest -> rest


idle :: Program
idle = Prog $ \t -> ([], Just t)

backward :: Double -> Program 
backward = forward . negate

left :: Double -> Program 
left = right . negate

-- Vector and matrix ops --

rotateRight :: Double -> Vector -> Vector
rotateRight d dir = undefined

(+++) :: Vector -> Vector -> Vector
(Vec x y) +++ (Vec x' y') = (Vec (x + x') (y + y'))  

(***) :: Vector -> Double -> Vector
(Vec x y) *** d = (Vec (x*d) (y*d))

infixl 5 ***
infixl 3 +++ 


