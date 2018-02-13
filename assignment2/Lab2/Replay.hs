-- | A module containing Replay progams, which keep a trace of computations and
-- can resume execution after halting.
module Replay(
  -- * Types
  Replay, Trace, Item (Result,Answer)
  -- * Derived
  , io
  , ask
  , addAnswer
  , emptyTrace  
  -- * Run
  , run
  ) where

import ReplayT
import Control.Monad (liftM,ap)

-- * Types
--------------------------------------------------------------------------------

-- | Replay type, using IO as the lower level monad for ReplayT
type Replay q r a = ReplayT IO q r a

-- * Operations
--------------------------------------------------------------------------------

-- | Execute IO action inside Replay program or load it from the trace if the
-- result is already in the trace
io :: (Show a, Read a) => IO a -> Replay q r a
io = action

-- | Pose question to user and stop the Replay program if no answer is found in
-- the trace.
ask :: q -> Replay q r r
ask = query

-- * Run function
--------------------------------------------------------------------------------

-- | Runs a Replay program
run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
run ra t = runReplayT ra t
