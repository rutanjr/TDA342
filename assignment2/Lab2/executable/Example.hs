{-# LANGUAGE OverloadedStrings #-}

module Main where

import Replay
import Web
import Web.Scotty (ActionM, ScottyM, scotty, get, post, rescue, html, param, params)
import Text.Read (readMaybe)

-- | Basically only runs our web program (starts scotty at port 3000 
-- and runs the example program)
main :: IO ()
main = scotty 3000 $ do
  get  "/" $ runWeb example
  post "/" $ runWeb example  



-- | Our exciting example program
example :: Web ()
example = do
  [name,color] <- ask ("Application to fancy website", zip ["Username:","Favorite color:"] (repeat Nothing))
  age <- ask ("All users must be at least 18",[("Age:", Just checkAge )])
  io (putStrLn $ "probably adding " ++ name ++ " to database")
  ask ("User " ++ name ++ " who likes the color " ++ color ++
       " has been accepted", [])
  return ()
  where
    checkAge :: String -> Bool
    checkAge age = case readMaybe age of
      Just age -> age >= 18
      _        -> False
  -- where
  --   tryAge :: String -> Web Int
  --   tryAge s = do
  --     [age] <- ask (s,["Age:"])
  --     case readMaybe age of
  --       Nothing  -> tryAge "Age must be an integer"
  --       Just age -> return age
