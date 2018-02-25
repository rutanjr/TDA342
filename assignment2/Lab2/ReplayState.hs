-- | Replay program using state as the lower level monad
module ReplayState where

import ReplayT
import Control.Monad.State

-- * Type
--------------------------------------------------------------------------------

-- | Replay program using State ase the lower level monad
type ReplayState s q r a = ReplayT (State s) q r a

-- * Operations
--------------------------------------------------------------------------------

-- | Run/lift a stateful computation in the Replay program
state :: (Show a, Read a) => (State s) a -> ReplayState s q r a
state sa = liftR sa -- action sa is the same thing

-- | Pose question to the user, stop program if no answer is found in the trace 
ask :: q -> ReplayState s q r r
ask q = query q

-- * Run function
--------------------------------------------------------------------------------

-- | Run the Replay program and return result wrapped in State monad
runReplayState :: ReplayState s q r a -> Trace r -> State s (Either (q, Trace r) a)
runReplayState rsa t = runReplayT rsa t

-- | Run a Replay program and then run the State monad
run :: ReplayState s q r a -> Trace r -> s -> (Either (q, Trace r) a, s)
run rsa t s = runState (runReplayState rsa t) s


