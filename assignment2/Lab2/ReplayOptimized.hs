{-# Language GADTs #-}

module ReplayOptimized(
  -- * Types
  Replay, Trace, Item (Result,Answer)
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
  AskBind :: q -> (r -> Replay q r a) -> Replay q r a
  IOBind  :: (Show a, Read a) => IO a -> (a -> Replay q r b) -> Replay q r b
  Return  :: a -> Replay q r a

-- | Adding the QA r first in the list.
type Trace r = [Item r] 

data Item r = Answer r | Result String
  deriving (Show,Read)

-- * Operations
--------------------------------------------------------------------------------

instance Monad (Replay q r) where 
  return = Return
  Return a >>= f = f a -- Left identity
  IOBind ma k >>= f = IOBind ma $ \a -> k a >>= f
  AskBind q k >>= f = AskBind q $ \r -> k r >>= f

io :: (Show a, Read a) => IO a -> Replay q r a
io ma = IOBind ma Return 

ask :: q -> Replay q r r
ask q = AskBind q Return

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

-- | Runs a Replay program with a trace 
run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
run (Return a)  t = return $ Right a
run (AskBind q k) t = case t of
  (Answer r:t') -> evalBind (Answer r) $ run (k r) t'
  _             -> return $ Left (q,t)
run (IOBind ma k) t = case t of
  [] -> do
    a <- ma
    evalBind (Result $ show a) $ run (k a) []
  (Result a:t) -> evalBind (Result a) $ run (k $ read a) t

-- | Evaluates the result of a run of a Replay program found in IOBind or
-- AskBind and returns the appropriate question and trace, or result.
evalBind :: Item r -> IO (Either (q, Trace r) a) -> IO (Either (q, Trace r) a)
evalBind i ma = do
  ea <- ma
  case ea of
    Left (q,t') -> return $ Left (q, i:t')
    _           -> return ea
  
-- * Monad magic
--------------------------------------------------------------------------------
instance Functor (Replay q r) where
    fmap = liftM
    
instance Applicative (Replay q r) where
    pure  = return
    (<*>) = ap
