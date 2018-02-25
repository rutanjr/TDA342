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
import Codec.Binary.Base64.String

-- * Data types
--------------------------------------------------------------------------------

type Web a = Replay Form Answers a
type Test a = Replay Int (Int -> Int) a

type Form = (String,[String])
type Answers = [String]

-- * Run
--------------------------------------------------------------------------------

runWeb :: Web a -> ActionM ()
runWeb webProg = do -- action land  
  t <- getTrace
  ta <- getAnswers t
  temp <- params
  liftIO $ putStrLn (show temp)
  r <- liftIO $ run webProg ta
  case r of
    Left (q,t') -> do -- action land
      html (fromForm t' q)
    r -> liftIO (putStrLn "OK") --return ()
  where    
    getTrace :: ActionM (Trace Answers)
    getTrace = rescue (do
                          t <- param "trace"
                          return . read . decode $ t                          
                      )
                      (\_ -> return emptyTrace)

    getAnswers :: Trace Answers -> ActionM (Trace Answers)
    getAnswers t = do
      input <- params
      let is = filter (("trace" /=) . fst) input
      case is of
        [] -> return t
        _  -> return $ addAnswer t $ map (unpack . snd) is

running :: Replay String String a -> IO a
running prog = play emptyTrace
 where
  play t = do
    r <- run prog t    -- this is the same prog every time!
    case r of
      Left (q,t2) -> do
        putStr ("Question: " ++ q ++ " ")
        r <- getLine
        play (addAnswer t2 r)
      Right x -> return x                   

fromForm :: Trace Answers -> Form -> Text
fromForm t (title,f) = pack $ mconcat $
  [ "<html><body>"
  , "<form method=post>"
--  , "<p>" ++ title ++ "</p>"
  , title
  , textFields
  , "<input type=submit value=OK>"
  , "<input type=hidden name=trace value="
  , (concat . lines . encode . show) t ++ ">"
  , "</form>"
  , "</body></html>"
  ]
  where
    textFields = concat $ zipWith textField f (map unpack inputNames)
    textField t l = "<p>" ++ t ++ "</p><input name=" ++ l ++ ">"
      
inputNames :: [Text]
inputNames = map (\n -> pack $ "input" ++ show n) [1..]

-- * Example
--------------------------------------------------------------------------------

main :: IO ()
main = scotty 3000 $ do
  get  "/" $ runWeb example
  post "/" $ runWeb example
  
example :: Web ()
example = do
  a1 <- ask ("Titletest",["Name?"])
  [s1,s2] <- ask ("",["Age?","Yes"])
  if (read s1 < 18)
    then (ask ("Titletest",["Must be over 18"]) >> example)
    else return ()
