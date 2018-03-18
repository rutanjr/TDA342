-- | A module containing Replay progams, which keep a trace of computations and
-- can resume execution after halting.
module Replay(
  -- * Types
  ReplayT, Replay, Trace, Item (Result,Answer)
  -- * Derived
  , io
  , ask
  , cut
  , addAnswer
  , emptyTrace  
  -- * Run
  , run
  , liftR
  ) where

import ReplayT
import Control.Monad (liftM,ap)

-- * Types
--------------------------------------------------------------------------------

-- | Replay type, using IO as the lower level monad for ReplayT
type Replay q r = ReplayT IO q r

-- * Operations
--------------------------------------------------------------------------------

-- | Execute IO action inside Replay program or load it from the trace if the
-- result is already in the trace
io :: (Show a, Read a) => IO a -> Replay q r a
io = action

-- | Pose question to user and stop the Replay program if no answer is found in
-- the trace.
ask :: Monad m =>  q -> ReplayT m q r r
ask = query

-- * Run function
--------------------------------------------------------------------------------

-- | Runs a Replay program
run :: Monad m => ReplayT m q r a -> Trace r -> m (Either (q, Trace r) a)
run ra t = runReplayT ra t
