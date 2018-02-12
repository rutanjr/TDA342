module Main where

import Data.IORef
import Replay
import System.Exit

-- | Runs the test suite for the replay library
main :: IO ()
main = do
  results <- runTests
  if and results
    then return ()
    else exitFailure

-- | Programs are parameterised over a 'tick' action.
--   Questions are () and answers are integers.
type Program = IO () -> Replay () Int Int

-- | A result is a pair of the final result of the program
--   and the number of 'ticks' executed.
type Result  = (Int, Int)
type Input   = [Int]

-- | A test case collects a program and a list of answers together
--   with its expected result.
data TestCase = TestCase
  { testName    :: String
  , testInput   :: Input
  , testResult  :: Result
  , testProgram :: Program
  }

-- | Running a program.
runProgram :: Program -> Input -> IO Result
runProgram p inp = do
    counter <- newIORef 0
    let tick = modifyIORef counter (+1)
    x <- play (p tick) emptyTrace inp
    n <- readIORef counter
    return (x, n)
  where
    play prog t inp = do
      r <- run prog t
      case r of
        Right x      -> return x
        Left (_, t') -> case inp of
          []       -> error "too few inputs"
          a : inp' -> play prog (addAnswer t' a) inp'

-- | Checking a test case. Compares expected and actual results.
checkTestCase :: TestCase -> IO Bool
checkTestCase (TestCase name i r p) = do
  putStr $ name ++ ": "
  r' <- runProgram p i
  if r == r'
    then putStrLn "ok" >> return True
    else putStrLn ("FAIL: expected " ++ show r ++
                  " instead of " ++ show r')
         >> return False
    

-- | List of interesting test cases.
testCases :: [TestCase]
testCases =
  [ TestCase
    { testName    = "test1"
    , testInput   = [3,4]
    , testResult  = (8, 1)
    , testProgram = \tick -> do
        io tick
        a <- ask () -- should be 3
        b <- io (return 1)
        c <- ask () -- should be 4
        return (a + b + c)
    } ,
    -- Testing a single return and empty trace
    TestCase
    { testName    = "test_return"
    , testInput   = []
    , testResult  = (0,0)
    , testProgram = \tick -> do
        io (return 0)
    } ,
    -- Testing that extra input is ignored
    TestCase
    { testName    = "test_extra_input"
    , testInput   = [1,2,3]
    , testResult  = (3,0)
    , testProgram = \tick -> do
        a <- ask ()
        b <- ask ()
        return (a + b)
    } ,
    -- Testing that bind behaves associatively
    TestCase
    { testName    = "test_bind_assoc"
    , testInput   = [0,1,0]
    , testResult  = (1,2)
    , testProgram = \tick -> ((ask () >> io tick) >> ask ()) >>=
                             (\a -> (io tick >> ask () >>= return (return a)))
    } ,
    -- Testing right identity of monads
    TestCase
    { testName    = "test_right_identity"
    , testInput   = [1]
    , testResult  = (1,0)
    , testProgram = \tick -> do
        ask () >>= return
    } ,
    -- Testing left identity of monads
    TestCase
    { testName    = "test_left_identity"
    , testInput   = []
    , testResult  = (1,0)
    , testProgram = \tick -> do
        return 0 >>= (return . succ)
    }        
  ]

-- | Running all the test cases.
runTests = mapM checkTestCase testCases

