import Codec.Pwdhash (pwdhash)
import System.Process (readProcess)
import Test.QuickCheck ((==>), Property, quickCheck)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

execNode :: String -> String -> IO String
execNode password realm = readProcess "node" ["js/pwdhash.js", password, realm] ""

noNull :: String -> Bool
noNull = all (/= '\NUL')

prop_regression :: String -> String -> Property
prop_regression password realm = noNull (password ++ realm) ==> prop
  where
    prop = monadicIO $ do
      expected <- run $ fmap init $ execNode password realm
      let actual = pwdhash password realm
      assert $ actual == expected

main :: IO ()
main = quickCheck prop_regression
