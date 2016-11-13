import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

import TextGen (TextGen, runTextGen, word, choose, remove, list, randrep, rep, perhaps, smartjoin, dumbjoin)
import System.Random


main :: IO ()
main = hspec $ do
  describe "word" $ do
    it "makes a generator which returns a literal" $ do
      literal <- return ( "Test" :: [ Char ] )
      g <- return $ word literal
      g1 <- getStdRandom $ runTextGen g
      result <- return $ dumbjoin g1
      result `shouldBe` literal
      




-- genWord :: [ Char ] -> IO [ Char ]
-- genWord w = do
--   g <- return $ word w
--   generate g
