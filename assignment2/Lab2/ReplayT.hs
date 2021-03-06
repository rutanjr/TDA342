{-# LANGUAGE GADTs #-}

-- | A monad transformer for Replay programs. Programs keep a trace of
-- computations and can resume execution where they halted. Execution
-- is halted by querying.
module ReplayT (
  -- * Types
  ReplayT, Trace, Item (Answer,Result)
  -- * Derived combinators
  , action
  , query
  , cut
  -- * Trace manipulation
  , emptyTrace
  , addAnswer
  -- * Run and lift functions
  , runReplayT
  , liftR
               ) where

import Control.Monad (liftM,ap)

-- * Types
--------------------------------------------------------------------------------

-- | Transformer for Replay programs
data ReplayT m q r a where
  QueryBind  :: q -> (r -> ReplayT m q r a) -> ReplayT m q r a
  ActionBind :: (Show a, Read a) => m a -> (a -> ReplayT m q r b) ->
                ReplayT m q r b
  Return     :: a -> ReplayT m q r a
  CutBind    :: (Show a, Read a) => ReplayT m q r a -> (a -> ReplayT m q r b) ->
                ReplayT m q r b

instance Monad (ReplayT m q r) where
  return          = Return
  Return a        >>= f = f a
  ActionBind ma f >>= g = ActionBind ma $ \a -> f a >>= g
  QueryBind  q  f >>= g = QueryBind  q  $ \r -> f r >>= g
  CutBind    ma f >>= g = CutBind    ma $ \a -> f a >>= g

-- | Trace of computations by a Replay program
type Trace r = [Item r] 

-- | A trace item is either the answer to a query or the result of a computation
data Item r = Answer r | Result String | ShortCut (Maybe String)
                                         -- ShortCut (Maybe String) ?
  deriving (Show,Read)

-- * Operation
--------------------------------------------------------------------------------

-- | Execute a monadic action on the Replay program.
action :: (Show a, Read a) => m a -> ReplayT m q r a
action ma = ActionBind ma Return

-- | Query the user for an answer by halting the Replay program.
query :: q -> ReplayT m q r r
query q = QueryBind q Return

cut :: (Monad m, Read a, Show a) => ReplayT m q r a -> ReplayT m q r a
cut ma = CutBind ma Return

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

-- | Runs a ReplayT program and wraps the result in the inner monad 'm'.
runReplayT :: Monad m => ReplayT m q r a -> Trace r -> m (Either (q, Trace r) a)
runReplayT (Return a) t = return $ Right a
runReplayT (QueryBind q k) t = case t of
  (Answer r:t') -> evalBind (Answer r) $ runReplayT (k r) t'
  _             -> return $ Left (q,t)
runReplayT (ActionBind ma k) t = case t of
  [] -> do
    a <- ma
    evalBind (Result $ show a) $ runReplayT (k a) []
  (Result a:t) -> evalBind (Result a) $ runReplayT (k $ read a) t
runReplayT (CutBind ma f) t = case t of
  (s@(ShortCut (Just a)):t') -> evalBind s $ runReplayT (f $ read a) t'
  (s@(ShortCut Nothing):t')  -> runShortCut ma t'
  _                -> runShortCut ma t
  where
    runShortCut ma t = do
      ea <- runReplayT ma t
      case ea of
        Left (q,t') -> return $ Left (q, ShortCut Nothing:t')
        Right a      -> evalBind (ShortCut $ Just (show a)) $ runReplayT (f a) []

-- | Evaluates the result of a run of a Replay program found in ActionBind or
-- QueryBind and returns the appropriate question and trace, or result.
evalBind :: Monad m => Item r -> m (Either (q, Trace r) a) -> m (Either (q, Trace r) a)
evalBind i ma = do
  ea <- ma
  case ea of
    Left (q,t') -> return $ Left (q, i:t')
    _           -> return ea

-- | Lifts a monadic action from the embedded monad into ReplayT by executing
-- the action with 'action'.
liftR :: (Monad m, Show a, Read a) => m a -> ReplayT m q r a
liftR ma = action ma

-- * Monad magic
--------------------------------------------------------------------------------
instance Functor (ReplayT m q r) where
    fmap = liftM
    
instance Applicative (ReplayT m q r) where
    pure  = return
    (<*>) = ap
