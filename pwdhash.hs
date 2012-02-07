import Codec.Binary.Base64
import Codec.Utils (Octet)
import Control.Applicative
import Control.Monad.State
import Data.HMAC
import System.Console.Haskeline
import System.Environment

toOctet :: String -> [Octet]
toOctet = map (fromIntegral . fromEnum)

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate (x:xt) n = rotate (xt ++ [x]) (n-1)

contains :: Char -> Char -> String -> Bool
contains from to s = any f s
  where
    f ch = ch >= from && ch <= to

isAlphaNum :: Char -> Bool
isAlphaNum ch = (ch >= 'A' && ch <= 'Z') ||
                (ch >= 'a' && ch <= 'z') ||
                (ch >= '0' && ch <= '9')

between :: Char -> Char -> Int -> Char
between from to offset = toEnum $ from' + offset `rem` (to' - from' + 1)
  where
    from' = fromEnum from
    to' = fromEnum to

type Extra = State [Int]

nextExtra :: Extra Int
nextExtra = do
  es <- get
  if null es
    then return 0
    else put (tail es) >> return (head es)

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

addNonAlphaNum :: Bool -> String -> Extra String
addNonAlphaNum f s = if f && (any (not . isAlphaNum) s)
                     then do ch <- nextExtraChar
                             return $ s ++ [ch]
                     else return $ s ++ ['+']

replaceNonAlphaNum :: String -> Extra String
replaceNonAlphaNum [] = return []
replaceNonAlphaNum (ch:ss) = if not $ isAlphaNum ch
                             then do ch' <- nextBetween 'A' 'Z'
                                     rest <- replaceNonAlphaNum ss
                                     return $ ch' : rest
                             else (ch :) <$> replaceNonAlphaNum ss

pwdhash :: String -> String -> String
pwdhash password realm = evalState (run prefix) extra
  where
    nonAlphaNum = any (not . isAlphaNum) password
    hash = encode $ hmac_md5 (toOctet password) (toOctet realm)
    size = length password + 2
    startingSize = size - 4
    (prefix, extra) = map fromEnum <$> splitAt startingSize hash
    run s = do s1 <- replace 'A' 'Z' s
               s2 <- replace 'a' 'z' s1
               s3 <- replace '0' '9' s2
               s4 <- addNonAlphaNum nonAlphaNum s3
               s5 <- replaceNonAlphaNum s4
               rotate s5 <$> nextExtra

main = do
  [realm] <- getArgs
  Just password <- runInputT defaultSettings $ getPassword (Just '*') "Password: "
  putStrLn $ pwdhash password realm
