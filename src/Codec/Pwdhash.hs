module Codec.Pwdhash (
  pwdhash
) where

import Codec.Binary.Base64 (encode)
import Codec.Utils (Octet)
import Control.Applicative ((<$>))
import Control.Monad.Trans.State (State, evalState, get, put)
import Data.HMAC (hmac_md5)
import Data.List (dropWhileEnd)

toOctets :: String -> [Octet]
toOctets = map (fromIntegral . fromEnum)

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate (x:xt) n = rotate (xt ++ [x]) (n-1)

contains :: Char -> Char -> String -> Bool
contains from to = any $ \ch -> ch >= from && ch <= to

isNotAlphaNum :: Char -> Bool
isNotAlphaNum ch = not $ (ch >= 'A' && ch <= 'Z') ||
                         (ch >= 'a' && ch <= 'z') ||
                         (ch >= '0' && ch <= '9') ||
                         (ch == '_')

between :: Char -> Char -> Int -> Char
between from to offset = toEnum $ from' + offset `rem` (to' - from' + 1)
  where
    from' = fromEnum from
    to' = fromEnum to

type Extra = State [Int]

nextExtra :: Extra Int
nextExtra = do
  es <- get
  case es of
    [] -> return 0
    e : et -> put et >> return e

nextExtraChar :: Extra Char
nextExtraChar = toEnum <$> nextExtra

nextBetween :: Char -> Char -> Extra Char
nextBetween from to = between from to <$> nextExtra

replace :: Char -> Char -> String -> Extra String
replace from to s = do
  ch <- if contains from to s
        then nextExtraChar
        else nextBetween from to
  return $ s ++ [ch]

addNonAlphaNum :: String -> Extra String
addNonAlphaNum s = if any isNotAlphaNum s
                   then do ch <- nextExtraChar
                           return $ s ++ [ch]
                   else return $ s ++ "+"

replaceNonAlphaNum :: String -> Extra String
replaceNonAlphaNum [] = return []
replaceNonAlphaNum (ch:ss) = if isNotAlphaNum ch
                             then do ch' <- nextBetween 'A' 'Z'
                                     rest <- replaceNonAlphaNum ss
                                     return $ ch' : rest
                             else (ch :) <$> replaceNonAlphaNum ss

pwdhash :: String -> String -> String
pwdhash password realm = evalState (run prefix) extra
  where
    nonAlphaNum = any isNotAlphaNum password
    hash = hmac_md5 (toOctets password) (toOctets realm)
    base64 = dropWhileEnd (== '=') $ encode hash
    size = length password + 2
    startingSize = size - 4
    (prefix, extra) = map fromEnum <$> splitAt startingSize base64
    run s = do s1 <- replace 'A' 'Z' s
               s2 <- replace 'a' 'z' s1
               s3 <- replace '0' '9' s2
               s4 <- if nonAlphaNum
                     then addNonAlphaNum s3
                     else return (s3 ++ "+")
               s5 <- if not nonAlphaNum
                     then replaceNonAlphaNum s4
                     else return s4
               rotate s5 <$> nextExtra
