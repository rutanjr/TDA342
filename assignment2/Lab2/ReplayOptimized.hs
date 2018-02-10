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
    Ask     :: q -> Replay q r r
    AskBind :: q -> (r -> Replay q r a) -> Replay q r a
    InOut   :: (Show a, Read a) => IO a -> Replay q r a
    IOBind  :: (Show a, Read a) => IO a -> (a -> Replay q r b) -> Replay q r b
    Return  :: a -> Replay q r a
    -- Combinators
    -- Removed bind
    Bind   :: (Replay q r) a -> (a -> (Replay q r) b) -> (Replay q r) b

-- | Adding the QA r first in the list.
type Trace r = [Item r] 

data Item r = Answer r | Result String
  deriving (Show,Read)

-- * Operations
--------------------------------------------------------------------------------

{-|

(>>=)  :: (Replay q r) a -> (a -> (Replay q r) b) -> (Replay q r) b
Ask    :: q -> Replay q r r
InOut  ::(Show a, Read a) => IO a -> Replay q r a
Return :: a -> Replay q r a

Left identity:
return a >>= f == f a
Right identity:
m >>= return == m
Associativity:
(m >>= f) >>= g == m >>= (\x -> f x >>= g)

-}

instance Monad (Replay q r) where 
    return = Return
    Return a >>= f = f a -- Left identity
    -- (>>=) :: (Replay q r) a -> (a -> (Replay q r) b) -> (Replay q r) b
    InOut ma >>= f = undefined -- think we are stuck, need trace
    IOBind ma k >>= f = IOBind ma $ \a -> k a >>= f
    -- (>>=) :: (Replay q r) r -> (r -> (Replay q r) b) -> (Replay q r) b
    Ask q    >>= f = undefined -- think we are stuck, need trace
    AskBind q k >>= f = AskBind q $ \r -> k r >>= f

io :: (Show a, Read a) => IO a -> Replay q r a
io ma = IOBind ma Return 
--io = InOut

ask :: q -> Replay q r r
ask q = AskBind q Return
--ask = Ask

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

{-|
Ask    :: q -> Replay q r r
InOut  :: (Show a, Read a) => IO a -> Replay q r a 
Return :: a -> Replay q r a 
Bind   :: (Replay q r) a -> (a -> (Replay q r) b) -> (Replay q r) b

Left identity:
return a >>= f == f a
Right identity:
m >>= return == m
Associativity:
(m >>= f) >>= g == m >>= (\x -> f x >>= g)
-}

-- | Runs a Replay program
run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
run (Ask q) t = case t of
  (Answer r:t') -> return $ Right r
  _             -> return $ Left (q,t)
run (InOut ma) t = case t of
  [] -> ma >>= return . Right
  (i:is) -> let (Result str) = i in return $ Right (read str)   
run (Return a)  t = return $ Right a
-- Bind changes the trace
-- run (Bind (Return a)  f) t = run (f a) t
-- run (Bind (Bind ra g) f) t = run (Bind ra (\x -> Bind (g x) f )) t -- Associativity of Monads
-- run (Bind (InOut ma)  f) t = do
--   -- Run IO
--   (Right b) <- run (InOut ma) t
--   -- Checking if trace should be extended
--   let t' = if null t
--            then [Result $ show b]
--            else t
--   -- Run the rest of the replay program
--   ea <- run (f b) (tail t')
--   -- Concatenate outputs if Left
--   case ea of
--     Left (q,t'') -> return $ Left (q,head t' : t'')
--     _            -> return ea  
-- run (Bind (Ask q)     f) t = do
--   eb <- run (Ask q) t
--   case eb of
--     Right b -> do
--       -- Run the rest of the replay program
--       ea <- run (f b) (tail t)
--       -- Concatenate outputs if Left
--       case ea of
--         Left (q,t') -> return $ Left (q,head t : t')
--         _           -> return ea        
--     -- Terminate run if no answer is found
--     Left p -> return $ Left p
--   where runRest b t = undefined

-- * Monad magic
--------------------------------------------------------------------------------
instance Functor (Replay q r) where
    fmap = liftM
    
instance Applicative (Replay q r) where
    pure  = return
    (<*>) = ap
