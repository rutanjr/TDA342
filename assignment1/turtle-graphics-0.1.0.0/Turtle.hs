{- |
Module      : Turtle
Description : Interface for writing turtle programs and a textual function to tun them.

The turtle interface provides primiteve, as well as derived, operators for creating turtle programs. The programs can be run with a textual function to produce a sequential list of performed actions.

-}
module Turtle (
  -- * The turtle types
  Program (P), Action, Vector, Line(from,to, lineclr), Pos, Dir
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
  , runLines
  , initTurtle

  ) where


import GHC.Exts
import Graphics.HGL hiding (Pen)
import Prelude hiding (lines)
import Data.Maybe (catMaybes)
import Data.List (uncons)
-- * 

-- | For directions and positions.
type Vector = (Double,Double)
-- | Position, contained in line and the turtle itself.
type Pos    = Vector
-- | Direction, to keep track of the state the turtle is in.
type Dir    = Vector
 
-- | A representation for the line drawn by a turtle, from - to as pos and color
data Line = Line
  { from    :: Pos
  , to      :: Pos
  , lineclr :: Color
  }
instance Show Line where
  show l = show (from l) ++ " - " ++ show (to l)

data Pen    = Pen
  { down :: Bool
  , clr  :: Color
  } deriving Show
  
data Turtle = Turtle
  { pos :: Vector
  , dir :: Vector
  , pen :: Pen
  } deriving Show


-- | Initiates a turtle at pos 0,0 facing east, that is drawing in black.
initTurtle :: Turtle
initTurtle = Turtle
  { pos = (0,0)
  , dir = (0,1)
  , pen = Pen True Black
  }

-- | This is a representation of an action of the turtle.
data Action = Act
  { counter   :: Int
  , operation :: Operation
  , turtle    :: Turtle
  } deriving Show
  

-- | Just to be able to let the action keep track of everything and not only 
-- the lines, messages to print in the runTextual ontop of the lines to be drawn
data Operation
  = Op  Line   -- ^ The constructor 'Op' for an action resulting in a line
  | Msg String -- ^ Constructor for turtle actions not resulting in lines
  deriving Show

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
newtype Program = P (Turtle -> Int -> ([Action],[Turtle]))

-- * Constructors
--------------------------------------------------------------------------------

-- | Moves turtle the specified distance in the direction it is facing 
forward :: Double -> Program
forward d = P $ \t n ->
  let t' = move d t
      operation | down (pen t) = Op  $ Line (pos t) (pos t') (clr . pen $ t) 
                | otherwise    = Msg $ "moved from " ++ show (pos t) ++ 
                                       " to " ++ show (pos t')    
  in ([Act (n+1) operation t'], [t'])

-- | Rotates turtle the specified amount of radians
right :: Double -> Program
right d = P $ \t n ->
  let t' = rotate d t in  ([Act (n+1) msg t'], [t'])
  where msg = if d /= 0
                then Msg $ "turned " ++ show d ++ " radians"
                else Msg "turtle idle"

-- | Lifts the pen resulting in no drawn lines 
penup :: Program
penup = P $ \t n ->
  let t' = t { pen = (pen t) { down = False}}
  in  ([Act (n+1) msg t'], [t'])
  where msg = Msg "lifted the pen"

-- | Puts the pen down and resumes drawing of lines
pendown :: Program
pendown = P $ \t n ->
  let t' = t { pen = (pen t) { down = True}}
  in  ([Act (n+1) msg t'], [t'])
  where msg = Msg "put pen down"
  
-- | Changes the color of the pen  
color :: Color -> Program
color c = P $ \t n -> 
  let t' = t { pen = (pen t) { clr = c}}
  in  ([Act (n+1) msg t'], [t'])
  where msg = Msg $ "changed color to " ++ show c
  
-- | Kills a turtle rendering it unable to perform more actions
die :: Program
die = P $ \t n -> ([Act (n+1) msg t], [])
  where msg = Msg "turtle died :("

-- * Combinators
--------------------------------------------------------------------------------

-- | Sequentialize two turtle programs. The second program will start where the
-- first one ended.
(>*>) :: Program -> Program -> Program
(P p1) >*> (P p2) = P $ \t n -> case p1 t n of
  (as1,[]) -> (as1,[])
  (as1,ts) -> (as1 ++ sortActions ass,ts')
    where m = counter . last $ as1
          os = map (`p2` m) ts
          ass = map fst os
          ass' = []
          ts' = concatMap snd os          
          
-- | Parallel combinator.
(<|>) :: Program -> Program -> Program
(P p1) <|> (P p2) = P $ \t n ->
  let (as1,ts1) = p1 t n
      (as2,ts2) = p2 t n
  in (sortActions [as1,as2],ts1 ++ ts2)


-- This is only to sort out the action lists. Only used internally here.
sortActions :: [[Action]] -> [Action]
sortActions []  = []
sortActions ass =
  case ass' of
    []-> []
    _ -> let n = counter $ head $ head ass'
             pairs = map (break ((>n) . counter)) ass'
             heads = concatMap fst pairs
             tails = map snd pairs
         in heads ++ sortActions tails
  where ass' = filter (not . null) ass
 
-- | Limits the program to the specified amount of actions. 
limited :: Int -> Program -> Program
limited m (P p) = P $ \t n ->
  let (as,ts) = p t n
  in case takeWhile ((<=m+n) . counter) as of
       [] -> ([],ts)
       as' -> let i = (counter . last) as'
                  ts' = map turtle $ dropWhile ((<i) . counter) as'
              in (as',ts')


-- | Repeats a program the specified amount of times.
times :: Int -> Program -> Program
times n p | n < 2     = p
          | otherwise = p >*> (times (n-1) p) 


-- * Derived Combinators
--------------------------------------------------------------------------------

-- | A program step where the turtle does nothing.
idle :: Program
idle = right 0

-- | Moves the turtle the distance backwards.
backward :: Double -> Program
backward = forward . negate

-- | Turning the turtle to the left. 
left :: Double -> Program
left = right . negate


-- | Repeats a program forever.
forever  :: Program -> Program
forever p = p >*> forever p

-- | The turtles deathtimer.
lifespan :: Int -> Program -> Program
lifespan n p = limited n p >*> die

-- * Run function
--------------------------------------------------------------------------------


-- | Runs the turtle and produces textual output.
runTextual :: Program -> IO ()
runTextual (P p) = 
  let (as,_) = p initTurtle 0
  in mapM_ (putStrLn . fromAction) as  
  where fromAction a = show (counter a) ++ ": " ++ fromOp a
        fromOp a =
          case operation a of
            Msg msg             -> msg
            Op (Line from to c) -> "drew line from " ++ show from ++
                                   " to " ++ show to 

-- | Runs the program and produces all the lines.
runLines :: Program -> [Line]
runLines (P p) =
  let (as,_) = p initTurtle 0
  in lines as

-- Internal to convertactions to lines (to be drawn).
lines :: [Action] -> [Line]
lines [] = []
lines (Act _ op _ : as) = case op of
  Op l -> l : lines as
  _    -> lines as

  
