module Main where

import Codec.Pwdhash (pwdhash)
import System.Console.Haskeline (defaultSettings, getPassword, runInputT)
import System.Environment (getArgs)

main :: IO ()
main = do
  [realm] <- getArgs
  Just password <- runInputT defaultSettings $ getPassword (Just '*') "Password: "
  putStrLn $ pwdhash password realm
