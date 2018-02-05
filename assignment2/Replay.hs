module Replay where


-- | Must be serializable. 
newtype Trace r = T r

-- | The monad for replay, in a deep embedding for now.
data Replay q r a where

	Ask 	:: q -> Replay q r a

	Return 	:: Replay q r a
	Bind 	:: Replay q r a


instance Monad (Replay q r) where
	return r = undefined
	(>>=) = undefined

io  :: (Show a, Read a) => IO a -> Replay q r a
io = undefined

ask :: q -> Replay q r r
ask = undefined

run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
run = undefined

-- Left (q, t)