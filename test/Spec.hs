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

  -- describe "quickcheck words" $ do
  --   it "tests that a word S output S for all S" $ property $ prop_literals


literal :: [ Char ] -> IO [ Char ]
literal w = do
  g <- return $ word w
  g1 <- getStdRandom $ runTextGen g
  return $ dumbjoin g1
  


-- prop_literals :: [ Char ] -> Property
-- prop_literals w = monadicIO $ do
--   w' <- literal w
--   assert ( w' == w )


-- genWord :: [ Char ] -> IO [ Char ]
-- genWord w = do
--   g <- return $ word w
--   generate g
