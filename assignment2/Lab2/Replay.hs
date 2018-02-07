{-# Language GADTs #-}

module Replay(
  -- * Types
  Replay, Trace, Item
  -- * Primitive
  , io
  , ask
  , addAnswer
  , emptyTrace
  -- * Derived
  
  -- * Run
  , run
  ) where

import Control.Monad (liftM,ap)

-- * Types
--------------------------------------------------------------------------------

data Replay q r a where
    -- Constructors
    Ask    :: q -> Replay q r r
    InOut  ::(Show a, Read a) => IO a -> Replay q r a
    Return :: a -> Replay q r a
    -- Combinators 
    Bind   :: (Replay q r) a -> (a -> (Replay q r) b) -> (Replay q r) b

-- | Adding the QA r first in the list.
type Trace r = [Item r] 

data Item r = Answer r | Result String
  deriving (Show,Read)

-- * Operations
--------------------------------------------------------------------------------
instance Monad (Replay q r) where 
    return = Return
    (>>=)  = Bind

io :: (Show a, Read a) => IO a -> Replay q r a
io a = InOut a

ask :: q -> Replay q r r
ask q = Ask q

-- * Trace manipulation
--------------------------------------------------------------------------------

-- | The empty trace, which is just and empty list
emptyTrace :: Trace r
emptyTrace = []

-- | Appends an answer to the end of a trace
addAnswer :: Trace r -> r -> Trace r
addAnswer t r = t ++ [Answer r]

-- * Run function
--------------------------------------------------------------------------------

--Ask    :: q -> Replay q r r
--InOut  :: (Show a, Read a) => IO a -> Replay q r a 
--Return :: a -> Replay q r a 
--Bind   :: (Replay q r) a -> (a -> (Replay q r) b) -> (Replay q r) b

-- | Runs a Replay program
run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
run (Ask q) t = case t of
  (Answer r:t') -> return $ Right r
  _             -> return $ Left (q,t)
run (InOut ma) t = case t of
  [] -> ma >>= return . Right
  (i:is) -> let (Result str) = i in return $ Right (read str)   
run (Return a)  t = return $ Right a
-- * does not work atm
run (Bind ra f) t = case ra of
  (InOut ma) -> do
    ea <- run (InOut ma) t
    let Right a = ea
    run (f a) $ t ++ [Result $ show a]
  _          -> do
    ea <- run ra t
    case ea of
      Right a     -> run (f a) $ t
      Left (q,t') -> return $ Left (q,t')

{- |
Might want a IOBind
-}

-- run (Bind ra f) t = do
--   ma <- run ra t
--   case ma of
--     Right a     -> run (f a) $ t
--     Left (q,t') -> return $ Left (q,t')

-- * TESTING
--------------------------------------------------------------------------------
prog_mini = io $ putStrLn "mini"

prog_uganda = ask "Do you know de wey?"

prog_test = do
  io $ putStrLn "1"
  io $ putStrLn "2"

prog_trace = do
  io $ putStrLn "1"
  io $ putStrLn "2"
  ask "Show me the trace?"

-- * Monad magic
--------------------------------------------------------------------------------
instance Functor (Replay q r) where
    fmap = liftM
    
instance Applicative (Replay q r) where
    pure  = return
    (<*>) = ap
