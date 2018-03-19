module Main where

import Data.IORef
import Control.Monad.State hiding (state)
import ReplayState 
import System.Exit
import Test.QuickCheck hiding (Result)

-- | Runs the test suite for the replay library
main :: IO ()
main = do
  results <- runTests
  if and results
    then return ()
    else exitFailure

-- | Programs are parameterised over a 'tick' action.
--   Questions are () and answers are integers.
type Program = State Int () -> ReplayState Int () Int Int

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
runProgram :: Program -> Input -> Result
runProgram p inp = runState (play (p tick) emptyTrace inp) 0
  where
    tick = modify (+1)
    play prog t inp = do
      r <- runReplayState prog t
      case r of
        Right x      -> return x
        Left (_, t') -> case inp of
          []       -> error "too few inputs"
          a : inp' -> play prog (addAnswer t' a) inp'

-- | Checking a test case. Compares expected and actual results.
checkTestCase :: TestCase -> IO Bool
checkTestCase test@(TestCase name i r p) = do
  putStr $ show test ++ ": "
  let r' = runProgram p i
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
        state tick
        a <- ask () -- should be 3
        b <- state (return 1)
        c <- ask () -- should be 4
        return (a + b + c)
    } ,
    -- Testing simple cut
    TestCase
    { testName    = "test_cut_return"
    , testInput   = []
    , testResult  = (0,0)
    , testProgram = \tick -> do
        cut (return 0)
    } ,
    -- Testing weird cut
    TestCase
    { testName  = "test_cut_inner"
    , testInput = [2]
    , testResult = (2,0)
    , testProgram = \tick -> do
        cut (cut (return 1) >> ask ())
    } ,
    -- Testing a single return and empty trace
    TestCase
    { testName    = "test_return"
    , testInput   = []
    , testResult  = (0,0)
    , testProgram = \tick -> do
        state (return 0)
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
    , testProgram = \tick -> ((ask () >> state tick) >> ask ()) >>=
                             (\a -> (state tick >> ask () >>= return (return a)))
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

-- * Quick check testing and generation of test cases
--------------------------------------------------------------------------------

instance Show TestCase where
  show t = "Test name: " ++ testName t ++ "\n" ++
           "Test input: " ++ show (testInput t) ++ "\n" ++
           "Test result: " ++ show (testResult t) ++ "\n" 

instance Arbitrary TestCase where
  arbitrary = rTestCases 15

rTestCases :: Int -> Gen TestCase
rTestCases n = frequency $ subCase n
  where
    subCase :: Int -> [(Int,Gen TestCase)]
    subCase n = [ (2, rAsk)
                , (2, rTick)
                , (1, rState)
                , (1, rRet)
                , (1, rCut (n - 1))
                , (n, rBind (n - 1))]
    -- | Representing cut
    rCut :: Int -> Gen TestCase
    rCut n = do
      c <- rTestCases n
      return TestCase { testName = "(c" ++ testName c ++ ")"
                      , testInput = testInput c
                      , testResult = testResult c
                      , testProgram = \tick -> cut $ testProgram c tick
                      }
    -- | Representing binds
    rBind :: Int -> Gen TestCase
    rBind n = do
      c1 <- rTestCases n 
      c2 <- rTestCases n
      return TestCase { testName = testName c1 ++ testName c2
                      , testInput = testInput c1 ++ testInput c2
                      , testResult =
                        let (r1,t1) = testResult c1
                            (r2,t2) = testResult c2
                        in (r1+r2,t1+t2)
                      , testProgram = \tick -> do
                          r1 <- testProgram c1 tick
                          r2 <- testProgram c2 tick
                          return (r1 + r2)
                  }
  
    -- | Representing questions and answers from user
    rAsk :: Gen TestCase
    rAsk = do
      i <- fmap abs arbitrary
      return TestCase { testName = "a"
                      , testInput = [i]
                      , testResult = (i,0)
                      , testProgram = \tick -> ask ()
                      }
    -- | Representing State actions with only side effects
    rTick :: Gen TestCase
    rTick = do
      return TestCase { testName = "t"
                      , testInput = []
                      , testResult = (0,1)
                      , testProgram = \tick -> state tick >> return 0
                      }
    -- | Representing State actions with only return values
    rState :: Gen TestCase
    rState = do
      n <- fmap abs arbitrary
      return TestCase { testName = "s"
                      , testInput = []
                      , testResult = (n,0)
                      , testProgram = \tick -> state (return n)
                      }
    -- | Representing monad returns
    rRet :: Gen TestCase
    rRet = do
      n <- fmap abs arbitrary
      return TestCase { testName = "r"
                      , testInput = []
                      , testResult = (n,0)
                      , testProgram = \tick -> return n
                      }
