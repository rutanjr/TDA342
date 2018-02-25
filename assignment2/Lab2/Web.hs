{-# LANGUAGE OverloadedStrings #-}

{- | Web module
-}
module Web (
  Web, Form, Answers
  , runWeb  
  ) where

import Replay
import Web.Scotty (ActionM, ScottyM, scotty, get, post, rescue, html, param, params)
import Data.Text.Lazy (Text, unpack, pack)
import Control.Monad.IO.Class (liftIO)
import Codec.Binary.Base64.String

-- * Data types
--------------------------------------------------------------------------------

-- | Just a shorthand for an instance of the replay monad.
type Web a = Replay Form Answers a

-- | Our form that has two parts, first the header as a String, and then
-- all of our questions as a list of strings.
type Form = (String,[String])

-- | Answers is just a list of strings, since that is what our input is.
type Answers = [String]

-- * Run
--------------------------------------------------------------------------------

-- | Runs our Web program. Extracts data and updates the web page (html) 
-- according to the programs wishes.
runWeb :: Web a -> ActionM ()
runWeb webProg = do -- action land  
  t <- getTrace
  ta <- getAnswers t
  r <- liftIO $ run webProg ta
  case r of
    Left (q,t') -> do -- action land
      html (fromForm t' q)
    r -> liftIO (putStrLn "OK") --return ()
  where    
    -- Gets the trace from the html page.
    getTrace :: ActionM (Trace Answers)
    getTrace = rescue (do
                          t <- param "trace"
                          return . read . decode $ t                          
                      )
                      (\_ -> return emptyTrace)
    -- Help function that takes all input from the page except the trace.
    getAnswers :: Trace Answers -> ActionM (Trace Answers)
    getAnswers t = do
      input <- params
      let is = filter (("trace" /=) . fst) input
      case is of
        [] -> return t
        _  -> return $ addAnswer t $ map (unpack . snd) is

-- | Takes our trace, a form, and creates our html page.
fromForm :: Trace Answers -> Form -> Text
fromForm t (title,f) = pack $ mconcat $
  [ "<html>"
  , "<head><title>" ++ title ++ "</title><h2>" ++ title ++ "</h2></head>"
  , "<body>"
  , "<form method=post>"
  , textFields
  , "<input type=submit value=OK>"
  , "<input type=hidden name=trace value="
  , (concat . lines . encode . show) t ++ ">"
  , "</form>"
  , "</body></html>"
  ]
  where
    textFields = concat $ zipWith textField f (map unpack inputNames)
    textField :: String -> String -> String
    textField t l = "<p>" ++ t ++ "</p><input name=" ++ l ++ ">"
      
-- | Just gives a list of names for our inputboxes, input1, input2, input3,... 
inputNames :: [Text]
inputNames = map (\n -> pack $ "input" ++ show n) [1..]
