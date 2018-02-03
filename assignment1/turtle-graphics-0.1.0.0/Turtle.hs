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

-- *
import GHC.Exts
import Data.List
-- * 

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

initTurtle :: Turtle
initTurtle = Turtle
  { pos = (0,0)
  , dir = (0,1)
  , pen = Pen True 0
  }

data Action = Act
  { counter   :: Int
  , operation :: Operation
  , turtle    :: Turtle
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
                | otherwise    = Msg $ "moved from " ++ show (pos t) ++ 
                                       " to " ++ show (pos t')    
  in ([Act (n+1) operation t'], Just t')

-- | Rotates turtle 'd' radians
right :: Double -> Program
right d = P $ \t n ->
  let t' = rotate d t in  ([Act (n+1) msg t'], Just t')
  where msg = if d /= 0
                then Msg $ "turned " ++ show d ++ " radians"
                else Msg $ "turtle idle"

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
                    in  (as1 ++ as2, mt)
  out            -> out
  
(<|>) :: Program -> Program -> Program
(P p1) <|> (P p2) = P $ \t n ->
  let (as1,mt1) = p1 t n
      (as2,mt2) = p2 t n
      ass1 = groupWith counter as1
      ass2 = groupWith counter as2
  in  (concat $ zipWith (++) ass1 ass2, Nothing)
  
limited :: Int -> Program -> Program
limited m (P p) = P $ \t n ->
  let (as,mt) = p t n
  in case takeWhile ((<=m+n) . counter) as of
       [] -> ([],mt)
       as -> if (counter . head) as < m
                then (as, mt)
                else (as, Just $  turtle . head $ as)
               
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
--------------------------------------------------------------------------------

runTextual :: Program -> IO ()
runTextual (P p) = 
  let (as,_) = p initTurtle 0
  in sequence_ $ map (putStrLn . fromAction) as  
  where fromAction a =
          case operation a of
            Msg msg      -> msg
            Op (Line from to c) -> "drew line from " ++ show from ++
                                   " to " ++ show to 

-- * Test programs
--------------------------------------------------------------------------------
prog_1 = forward 2
prog_2 = right 0 >*> right 1
prog_3 = right (pi/2) >*> penup >*> forward 1 >*> pendown >*> forward 1
prog_forever = forever idle
prog_not_forever = idle >*> die >*> forever idle
