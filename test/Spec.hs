import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

import TextGen (TextGen, runTextGen, word, choose, remove, list, randrep, rep, perhaps, smartjoin, dumbjoin)
import System.Random


main :: IO ()
main = hspec $ do
  describe "word" $ do
    it "makes a generator which returns a literal" $ do
      w <- return ( "Test" :: [ Char ] )
      w' <- literal w
      w' `shouldBe` w

  describe "quickcheck words" $ do
    it "(word S) returns S" $ property $ prop_literals

  describe "list" $ do
    it "(list [ w1, w2, w3, .. ]) returns w1 w2 w3 .." $ property $ prop_list




literal :: [ Char ] -> IO [ Char ]
literal w = do
  g <- return $ word w
  g1 <- getStdRandom $ runTextGen g
  return $ dumbjoin g1
  
prop_literals :: [ Char ] -> Property
prop_literals w = monadicIO $ do
  w' <- run $ literal w
  assert ( w' == w )


listconcat :: [ [ Char ] ] -> IO [ Char ]
listconcat ws = do
  g <- return $ list $ map word ws
  g1 <- getStdRandom $ runTextGen g
  return $ dumbjoin g1

prop_list :: [ [ Char ] ] -> Property
prop_list ws = monadicIO $ do
  ws' <- run $ listconcat ws
  assert ( ws' == dumbjoin ws )
