{-# LANGUAGE OverloadedStrings #-}

{- | Web module
-}
module Web (
  Web
  , runWeb
  
  ) where

import Web.Scotty     (ActionM, ScottyM, scotty, get, post, put, rescue, html, param, params, text)
import Data.Text.Lazy (Text, unpack, pack)
import Replay
import Control.Monad.IO.Class (liftIO)

-- * Data types
--------------------------------------------------------------------------------

type Web a = Replay Form Answers a
type Test a = Replay Int (Int -> Int) a

type Form = [String]
type Answers = [String]

mkForm :: Form
mkForm = undefined

-- * Run
--------------------------------------------------------------------------------

runWeb :: Web a -> ActionM ()
runWeb webProg = web emptyTrace
  where
    web :: Trace Answers -> ActionM ()
    web t = do -- action land
      r <- liftIO $ run webProg t
      case r of
        Left (q,t') -> do -- action land
          html $ fromForm q
          rescue
            (do
                is <- sequence $ map param $ take (length q) $ inputnames
                web $ addAnswer t' (map unpack is))
            (\_ -> web t')
        Right x     -> return ()


fromForm :: Form -> Text
fromForm f = mconcat $
  [ "<html><body>"
  , "<form method=post>"]
  ++ textFields ++
  [ "<input type=submit value=OK>"
  , "</form>"
  , "</body></html>"
  ]
  where
    textFields = map pack $ concat $ zipWith
      (\text label -> ["<p>" ++ text ++ "</p>",
                       "<input name=" ++ label ++ ">"])
      f $ map unpack inputnames
      
inputnames :: [Text]
inputnames = map (\n -> pack $ "input" ++ show n) [1..]

-- * Example
--------------------------------------------------------------------------------

main :: IO ()
main = scotty 3000 $ do
  get "/" $ runWeb example
  post "/" $ runWeb example

example :: Web ()
example = do
  s <- ask ["String here", "and here"]
  io $ sequence $ map putStrLn s
  return ()

ageForm :: Form
ageForm = undefined
