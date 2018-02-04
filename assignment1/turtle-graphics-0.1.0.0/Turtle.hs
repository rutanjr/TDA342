{- |
Module      : Turtle
Description : Interface for writing turtle programs and a textual function to tun them.

The turtle interface provides primiteve, as well as derived, operators for creating turtle programs. The programs can be run with a textual function to produce a sequential list of performed actions.

-}
module Turtle (
  -- * The turtle types
  Program (P), Action, Vector, Line(from,to), Pos, Dir
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

-- *
import GHC.Exts
import Prelude hiding (lines)
import Data.Maybe (catMaybes)
import Data.List (uncons)
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
  } deriving Show
  
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

-- | Moves turtle a distance 'd' in the direction it is facing 
forward :: Double -> Program
forward d = P $ \t n ->
  let t' = move d t
      operation | down (pen t) = Op  $ Line (pos t) (pos t') (clr . pen $ t) 
                | otherwise    = Msg $ "moved from " ++ show (pos t) ++ 
                                       " to " ++ show (pos t')    
  in ([Act (n+1) operation t'], [t'])

-- | Rotates turtle 'd' radians
right :: Double -> Program
right d = P $ \t n ->
  let t' = rotate d t in  ([Act (n+1) msg t'], [t'])
  where msg = if d /= 0
                then Msg $ "turned " ++ show d ++ " radians"
                else Msg $ "turtle idle"

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

infixl 9 >*>
infixl 8 <|>

-- | Sequentialize two turtle programs. The second program will start where the
-- first one ended.
(>*>) :: Program -> Program -> Program
(P p1) >*> (P p2) = P $ \t n -> case p1 t n of
  (as,[]) -> (as,[])
  (as1,ts) ->
    let m = counter . last $ as1
        os = map (\t -> p2 t m) ts -- ska inte vara 'n'
        ass = map fst os
        asss = map (groupWith counter) ass
        ts' = concat $ map snd os        
    in (as1 ++ zipActions asss,ts')

-- | Parallel
(<|>) :: Program -> Program -> Program
(P p1) <|> (P p2) = P $ \t n ->
  let (as1,ts1) = p1 t n
      (as2,ts2) = p2 t n
      ass1 = groupWith counter as1
      ass2 = groupWith counter as2
  in (zipActions [ass1,ass2], ts1 ++ ts2)

zipActions :: [[[Action]]] -> [Action]
zipActions []     = []
zipActions [[]]   = []
zipActions groups = concat heads ++ zipActions tails
  where pairs = catMaybes $ map uncons groups
        heads = map fst pairs
        tails = map snd pairs

-- | Limited
limited :: Int -> Program -> Program
limited m (P p) = P $ \t n ->
  let (as,ts) = p t n
  in case takeWhile ((<=m+n) . counter) as of
       [] -> ([],ts)
       as' -> let i = (counter . last) as'
                  ts' = map turtle $ dropWhile ((<i) . counter) as'
              in (as',ts')
               
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
  where fromAction a = show (counter a) ++ ": " ++ fromOp a
        fromOp a =
          case operation a of
            Msg msg             -> msg
            Op (Line from to c) -> "drew line from " ++ show from ++
                                   " to " ++ show to 

runLines :: Program -> [Line]
runLines (P p) =
  let (as,_) = p initTurtle 0
  in lines as

lines :: [Action] -> [Line]
lines [] = []
lines (Act _ op _ : as) = case op of
  Op l -> l : lines as
  _    -> lines as


-- * Test programs
--------------------------------------------------------------------------------
prog_1 = forward 2
prog_2 = right 0 >*> right 1
prog_3 = right (pi/2) >*> penup >*> forward 1 >*> pendown >*> forward 1
prog_forever = forever idle
prog_not_forever = idle >*> die >*> forever idle
  
