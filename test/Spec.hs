import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

import TextGen (
  TextGen
  , runTextGen
  , word
  , aan
  , choose
  , chooseN
  , remove
  , list
  , randrep
  , rep
  , perhaps
  , smartjoin
  , dumbjoin
  )
import System.Random

import Data.List (intercalate)
import Control.Monad (forM)

type TextGenCh = TextGen StdGen [[Char]]




main :: IO ()
main = hspec $ do
  describe "word" $ do
    it "checks (word S) returns S" $ property $ prop_literals

  describe "aan" $ do
    it "checks (aan $ choose [s]) has good articles" $ property $ prop_aans

  describe "list" $ do
    it "checks that (list ws) = concat ws" $ property $ prop_list

  describe "choose" $ do
    it "checks (choose ws) returns a member of ws" $ property $ prop_choose 

  describe "remove" $ do
    it "checks (remove ws) removes one and returns the rest" $ property $ prop_remove

  describe "chooseN" $ do
    it "checks (chooseN n ws) returns n elements from ws" $ property $ prop_chooseN

  describe "perhaps" $ do
    it "checks that perhaps works" $ property $ prop_perhaps

  describe "rep" $ do
    it "checks that rep creates N copies" $ property $ prop_rep

  describe "randrep" $ do
    it "checks that randrep creates (N, M) copies" $ property $ prop_randrep


  
prop_literals :: [ Char ] -> Property
prop_literals w = monadicIO $ do
  w' <- run $ do
    g <- return $ word w
    g1 <- getStdRandom $ runTextGen g
    return $ dumbjoin g1 
  assert ( w' == w )




prop_aans :: NonEmptyList [ Char ] -> Property
prop_aans (NonEmpty ws) = monadicIO $ do
  ws' <- run $ do
    g <- return $ aan $ TextGen.choose $ map word ws
    g1 <- getStdRandom $ runTextGen g
    return $ g1
  assert $ correct_article ws'

correct_article :: [ [ Char ] ] -> Bool
correct_article (x:y:zs) = case mfirst y of
  Nothing -> True
  Just c -> case c `elem` "aeiouAEIOU" of
    True -> ( x == "an" )
    False -> ( x == "a" )
correct_article _ = False
      
mfirst :: [ Char ] -> Maybe Char
mfirst []   = Nothing
mfirst (c:cs) = Just c


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


prop_chooseN :: Positive Int -> [ [ Char ] ] -> Property
prop_chooseN (Positive n) ws = monadicIO $ do
  ws' <- run $ do
    g <- return $ chooseN (map word ws) n
    choices <- getStdRandom $ runTextGen g
    flip mapM choices $ \w -> do
      g' <- getStdRandom $ runTextGen w
      return $ dumbjoin g'
  l <- return $ length ws
  l' <- return $ length ws'
  assert $ if l < n then l' == l else l' == n
  assert $ all (\w -> ( w `elem` ws )) ws'



genlist :: [ TextGenCh ] -> IO [ Char ]
genlist ws = do
  results <- flip mapM ws $ \w -> do
    g <- getStdRandom $ runTextGen w
    return $ dumbjoin g
  return $ "[" ++ (intercalate ", " results) ++ "]"

nperhaps :: Int
nperhaps = 50000

tolerance :: Float
tolerance = 0.2

prop_perhaps :: Positive Int -> Positive Int -> [ Char ] -> Property
prop_perhaps (Positive n0) (Positive m0) w = monadicIO $ do
  ( n, m ) <- return $ if ( n0 < m0 ) then ( n0, m0 ) else ( m0, n0 )
  results <- run $ do
    g <- return $ perhaps ( n, m ) (word w)
    forM [1..nperhaps] $ \_ -> do
      getStdRandom $ runTextGen g
  empties <- return $ length $ filter null results
  ratio <- return $ (fromIntegral n / fromIntegral m)
  got <- return $ ((fromIntegral (nperhaps - empties)) / fromIntegral nperhaps)
  assert ( (abs (got - ratio)) < tolerance )

prop_rep :: Positive Int -> [ Char ] -> Property
prop_rep (Positive n) w = monadicIO $ do
  repeats <- run $ do
    g <- return $ rep n (word w)
    getStdRandom $ runTextGen g
  expected <- return $ intercalate " " ( take n $ repeat w )
  assert $ expected == (dumbjoin repeats)


prop_randrep :: Positive Int -> Positive Int -> [ Char ] -> Property
prop_randrep (Positive a0) (Positive b0) w = monadicIO $ do
  ( a, b ) <- return $ if a0 < b0 then ( a0, b0 ) else ( b0, a0 )
  repeats <- run $ do
    g <- return $ randrep ( a, b ) (word w)
    getStdRandom $ runTextGen g
  l <- return $ length repeats
  expected <- return $ intercalate " " ( take l $ repeat w )
  assert $ expected == (dumbjoin repeats)
  assert ( ( l >= a ) && ( l <= b ) )
