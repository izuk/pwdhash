import Codec.Pwdhash (pwdhash)
import Data.Char (isAscii, isPrint)
import System.Process (readProcess)
import Test.Framework (Test, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (listOf, suchThat)
import Test.QuickCheck.Monadic (assert, monadicIO, run)


newtype S = S String
  deriving (Eq, Ord, Show, Read)

okS :: Char -> Bool
okS s = isAscii s && isPrint s

instance Arbitrary S where
  arbitrary = S <$> (listOf $ arbitrary `suchThat` okS)
  shrink (S s) = S <$> shrink s

execNode :: String -> String -> IO String
execNode password realm = readProcess "node" ["js/pwdhash.js", password, realm] ""

prop_regression :: S -> S -> Property
prop_regression password' realm' = monadicIO $ do
  let S password = password'
      S realm = realm'
  run $ print (password, realm)
  expected <- run $ fmap init $ execNode password realm
  let actual = pwdhash password realm
  assert $ actual == expected

tests :: [Test]
tests =
  [ testProperty "regression" prop_regression
  ]

main :: IO ()
main = defaultMain tests
