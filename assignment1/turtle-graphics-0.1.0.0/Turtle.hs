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
type Pos    = Vector
type Dir    = Vector
type Color  = Int -- placeholder

data Line = Line
  { from    :: Pos
  , to      :: Pos
  , lineclr :: Color
  }

data Pen    = Pen
  { down :: Bool
  , clr  :: Color
  }
  
data Turtle = Turtle
  { pos :: Vector
  , dir :: Vector
  , pen :: Pen
  }

data Action = Act
  { counter :: Int
  , op      :: Operation
  , turtle  :: Turtle
  }
  
data Operation
  = Op  Line   -- ^ The constructor 'Op' for an action resulting in a line
  | Msg String -- ^ Constructor for turtle actions not resulting in lines

-- * Internal functions
--------------------------------------------------------------------------------

-- | Moves a turtle in its direction 'd' steps
move :: Double -> Turtle -> Turtle
move d t = t { pos = (x + d * dx, y + d * dy)}
  where (x,y)   = pos t
        (dx,dy) = dir t

-- | Rotate vector, basically matrix multiplication with 2d rotation matrix
rotate :: Double -> Turtle -> Turtle
rotate d t = t { dir = (cos d * x + sin d * y, cos d * y - sin d * x)}
  where (x,y) = dir t

-- * Turtle data type and functions
--------------------------------------------------------------------------------

-- | A program is a function which takes a Turtle and returns a list of actions
-- performed and maybe a turtle, depending on its survival.
newtype Program = P (Turtle -> Int -> ([Action],Maybe Turtle))
-- * P = (Turtle -> {Lines})
-- P1 == P2  <=> (t :: Turtle => P1 t == P2 t)


-- * Constructors

-- | Moves turtle a distance 'd' in the direction it is facing 
forward :: Double -> Program
forward d = P $ \t n ->
  let t' = move d t
      operation | down (pen t) = Op  $ Line (pos t) (pos t') (clr . pen $ t) 
                | otherwise    = Msg $ "moved " ++ show d ++ " in direction "
                                                ++ show  (dir t)    
  in ([Act (n+1) operation t'], Just t')

-- | Rotates turtle 'd' radians
right :: Double -> Program
right d = P $ \t n ->
  let t' = rotate d t in  ([Act (n+1) msg t'], Just t')
  where msg = Msg $ "turned " ++ show d ++ " radians"    

-- | Lifts the pen resulting in no drawn lines 
penup :: Program
penup = P $ \t n ->
  let t' = t { pen = (pen t) { down = False}}
  in  ([Act (n+1) msg t'], Just t')
  where msg = Msg "lifted the pen"

-- | Puts the pen down and resumes drawing of lines
pendown :: Program
pendown = P $ \t n ->
  let t' = t { pen = (pen t) { down = True}}
  in  ([Act (n+1) msg t'], Just t')
  where msg = Msg "put pen down"
  
-- | Changes the color of the pen  
color :: Color -> Program
color c = P $ \t n -> 
  let t' = t { pen = (pen t) { clr = c}}
  in  ([Act (n+1) msg t'], Just t')
  where msg = Msg $ "changed color to " ++ show c
  
-- | Kills a turtle rendering it unable to perform more actions
die :: Program
die = P $ \t n -> ([Act (n+1) msg t], Nothing)
  where msg = Msg "turtle died :("

-- * Combinators

-- | Sequentialize two turtle programs. The second program will start where the
-- first one ended. 
(>*>) :: Program -> Program -> Program
(P p1) >*> (P p2) = P $ \t n -> case p1 t n of
  ([],  Just t1) -> p2 t1 n
  (as1, Just t1) -> let (as2, mt) = p2 t1 $ counter . head $ as1
                    in  (as2 ++ as1, mt)
  out            -> out
  
(<|>) :: Program -> Program -> Program
(P p1) <|> (P p2) = undefined

limited :: Int -> Program -> Program
limited m (P p) = P $ \t n ->
  let (as,mt) = p t n
  in  (takeWhile ((<=m+n) . counter) as, )

{-*
  limited 0 : t -> ([], Just t)
  limited n : t -> ([t1,...,tm], Maybe tm)
  forward n >*> limited 0 (forever idle) >*> forward n =?= forward n >*> forward n
-}

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
