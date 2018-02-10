{-# LANGUAGE GADTs #-}
module ReplayT where

import Control.Monad (liftM,ap)
import Control.Monad.Trans


-- * Types
--------------------------------------------------------------------------------

-- * type Replay q r a = ReplayT IO q r a
-- * liftR :: (Monad m, Show a, Read a) => m a -> ReplayT m q r a

--newtype ReplayT m q r a = ReplayT {
--  runReplayT :: ReplayT m q r a -> Trace r ->  m (Either (q, Trace r) a)
--  }

data ReplayT m q r a where
  QueryBind  :: q -> (r -> ReplayT m q r a) -> ReplayT m q r a
  ActionBind :: (Show a, Read a) => m a -> (a -> ReplayT m q r b) -> ReplayT m q r b
  Return     :: a -> ReplayT m q r a

instance Monad (ReplayT m q r) where
  return          = Return
  Return a        >>= f = f a
  ActionBind ma k >>= f = ActionBind ma $ \a -> k a >>= f
  QueryBind q k   >>= f = QueryBind q $ \r -> k r >>= f

-- instance MonadTrans (ReplayT m q r) where
--  lift = undefined

type Trace r = [Item r] 

data Item r = Answer r | Result String
  deriving (Show,Read)

-- * Operation
--------------------------------------------------------------------------------

action :: (Show a, Read a) => m a -> ReplayT m q r a
action ma = ActionBind ma Return

query :: q -> ReplayT m q r r
query q = QueryBind q Return

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

runReplayT :: ReplayT m q r a -> Trace r -> m (Either (q, Trace r) a)
runReplayT = undefined

liftR :: (Monad m, Show a, Read a) => m a -> ReplayT m q r a
liftR = undefined

-- -- | Runs a Replay program with a trace 
-- run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
-- run (Return a)  t = return $ Right a
-- run (AskBind q k) t = case t of
--   (Answer r:t') -> evalBind (Answer r) $ run (k r) t'
--   _             -> return $ Left (q,t)
-- run (IOBind ma k) t = case t of
--   [] -> do
--     a <- ma
--     evalBind (Result $ show a) $ run (k a) []
--   (Result a:t) -> evalBind (Result a) $ run (k $ read a) t

-- -- | Evaluates the result of a run of a Replay program found in IOBind or
-- -- AskBind and returns the appropriate question and trace, or result.
-- evalBind :: Item r -> IO (Either (q, Trace r) a) -> IO (Either (q, Trace r) a)
-- evalBind i k = do
--   ea <- k
--   case ea of
--     Left (q,t') -> return $ Left (q, i:t')
--     _           -> k
  
-- * Monad magic
--------------------------------------------------------------------------------
instance Functor (ReplayT m q r) where
    fmap = liftM
    
instance Applicative (ReplayT m q r) where
    pure  = return
    (<*>) = ap
