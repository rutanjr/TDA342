{-# Language GADTs #-}
module Replay where


-- Types

data Replay q r a where
    -- Constructors
    Ask         :: q -> Replay q r a
    InOu        :: IO a -> Replay q r a
    Return      :: Replay q r a
    -- Combinators 
    Bind        :: Replay q r a -> (a -> Replay q r b) -> Replay q r b

    -- What?! 
    Left        :: (q, Trace r) -> Replay q r a
    Right       :: x -> Replay q r a
    -- Run function
    Run         :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)


-- | Adding the QA r first in the list.
type Trace r = [QA r] 

data QA r = Q r | A r 


instance Show (QA r) where 
    show (Q r) = show r ++ "?"
    show (A r) = show r ++ "."


emptyTrace :: Trace r
emptyTrace = []


-- Operations
instance Monad (Replay q r) where 
    return = Return
    (>>=) = Bind

-- instance Show r => Show (Trace r)
-- instance Read r => Read (Trace r)


io  :: (Show a, Read a) => IO a -> Replay q r a
io a = InOu a

ask :: q -> Replay q r r
ask q = Ask q

addAnswer  :: Trace r -> r -> Trace r
addAnswer t r = r ++ t

run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
run = undefined

instance Functor (Replay q r) where
    fmap = liftM

instance Applicative (Replay q r) where
    pure           = Just
    Nothing <*> vv = Nothing
    Just f  <*> vv = fmap f vv 



