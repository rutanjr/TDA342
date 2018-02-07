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

emptyTrace :: Trace r
emptyTrace = []

addAnswer :: Trace r -> r -> Trace r
addAnswer t r = Answer r : t

-- * Run function
--------------------------------------------------------------------------------

-- Ask    :: q -> Replay q r a
-- InOut   :: IO a -> Replay q r a
-- Return :: a -> Replay q r a
-- Bind   :: Replay q r a -> (a -> Replay q r b) -> Replay q r b

run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
run (Ask q)    t      = return $ Left (q,t)
run (InOut io) []     = do
  a <- io
  return temp
  where temp = undefined
 
run (InOut io) (i:is) = undefined
  
run (Return a) t = return (Right a)
run (Bind a f) t = undefined

-- * Monad magic
--------------------------------------------------------------------------------

instance Functor (Replay q r) where
    fmap = liftM

instance Applicative (Replay q r) where
    pure  = return
    (<*>) = ap
