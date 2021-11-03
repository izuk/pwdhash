import Codec.Pwdhash (pwdhash)
import System.Process (readProcess)
import Test.Framework (Test, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck ((==>), Property)
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

tests :: [Test]
tests =
  [ testProperty "regression" prop_regression
  ]

main :: IO ()
main = defaultMain tests
