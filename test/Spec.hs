import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

import TextGen (TextGen, runTextGen, word, choose, remove, list, randrep, rep, perhaps, smartjoin, dumbjoin)
import System.Random

import Data.List (intercalate)

type TextGenCh = TextGen StdGen [[Char]]




main :: IO ()
main = hspec $ do
  describe "word" $ do
    it "checks (word S) returns S" $ property $ prop_literals

  describe "list" $ do
    it "checks that (list ws) = concat ws" $ property $ prop_list

  describe "choose" $ do
    it "checks (choose ws) returns a member of ws" $ property $ prop_choose 

  describe "remove" $ do
    it "checks (remove ws) removes one and returns the rest" $ property $ prop_remove

  
prop_literals :: [ Char ] -> Property
prop_literals w = monadicIO $ do
  w' <- run $ do
    g <- return $ word w
    g1 <- getStdRandom $ runTextGen g
    return $ dumbjoin g1 
  assert ( w' == w )


prop_list :: [ [ Char ] ] -> Property
prop_list ws = monadicIO $ do
  ws' <- run $ do
    g <- return $ list $ map word ws
    g1 <- getStdRandom $ runTextGen g
    return $ dumbjoin g1
  assert ( ws' == dumbjoin ws )

-- choose is unsafe and can't handle empty option lists

prop_choose :: NonEmptyList [ Char ] -> Property
prop_choose (NonEmpty ws) = monadicIO $ do
  w' <- run $ do
    g <- return $ TextGen.choose $ map word ws
    g1 <- getStdRandom $ runTextGen g
    return $ dumbjoin g1
  assert (w' `elem` ws)

prop_remove :: [ [ Char ] ] -> Property
prop_remove ws = monadicIO $ do
  ( mw', ws' ) <- run $ do
    g <- return $ remove $ map word ws
    ( mg1, ws' ) <- getStdRandom $ runTextGen g
    sl' <- genlist ws'
    case mg1 of
      Nothing -> do
        return ( Nothing, ws' )
      Just g1 -> do
        w <- return $ dumbjoin g1
        return ( Just w, ws' )
  remainder <- run $ flip mapM ws' $ \w -> do
    g <- getStdRandom $ runTextGen w
    return $ dumbjoin g
  case mw' of
    Nothing -> assert True
    Just w' -> assert ( ( w' `elem` ws ) && (countin remainder == (countin ws) - 1 ) )
      where countin l = length $ filter (== w') l


genlist :: [ TextGenCh ] -> IO [ Char ]
genlist ws = do
  results <- flip mapM ws $ \w -> do
    g <- getStdRandom $ runTextGen w
    return $ dumbjoin g
  return $ "[" ++ (intercalate ", " results) ++ "]"
