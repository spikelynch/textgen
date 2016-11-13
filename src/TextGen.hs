module TextGen (
  TextGen
  ,runTextGen
  ,word
  ,choose
  ,remove
  ,list
  ,perhaps
  ,rep
  ,randrep
  ,smartjoin
  ,dumbjoin
  ,upcase
  ,loadOptions
  ,loadList
  ) where

import Control.Applicative
import Control.Monad (liftM, ap, forM_)
import Data.List (intercalate)
import Data.Char (toUpper)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import System.Random


newtype TextGen s a = TextGen { runTextGen :: s -> ( a, s ) }

instance (RandomGen s) => Monad (TextGen s) where
  return x = TextGen $ \s -> ( x, s )
  (TextGen h) >>= f = TextGen $ \s -> let ( a, newTextGen ) = h s
                                          (TextGen g) = f a
                                      in g newTextGen
                                     
                                     

instance (RandomGen s) => Functor (TextGen s) where
  fmap = liftM
  
instance (RandomGen s) => Applicative (TextGen s) where
  pure = return
  (<*>) = ap    

-- Basic combinator functions
  
-- (word "literal") -> a TextGen which returns [ "literal" ]

-- word returns [ "chars" ] so that empty results can be collapsed
-- easily when joining the results up

word :: (RandomGen g) => [Char] -> TextGen g [[Char]]
word a = return [ a ]

-- (choose [ TextGen ]) -> choose one of the TextGens in the list
                              
-- TODO: chooseN (get multiple items from the same list without
-- repeats)

choose :: (RandomGen g) => [ TextGen g a ] -> TextGen g a
choose options = TextGen $ \s -> let ( i, s' ) = randomR (0, (length options) - 1 ) s
                                     (TextGen optf) = options !! i
                                 in optf s'


-- for choose-and-remove: remember that a state combinator is a
-- lambda which wraps one or more existing combinators

-- so remove needs to -
--   - extract a random option from the list of options
--   - return that option and a new combinator with the new list
--   - to get the new combinator it has to call itself with the new list

removel :: Int -> [ a ] -> (  [ a ], [ a ] )
removel i l = let ( h, t ) = splitAt i l
          in case ( h, t ) of
               ( _, [] ) -> ( [], l )
               ( h, t:ts ) -> ( [ t ], h ++ ts )


remove :: (RandomGen g) => [ TextGen g a ] -> TextGen g ( Maybe a, [ TextGen g a ] )
remove options = TextGen $ \s -> let ( i, s1 ) = randomR (0, (length options) - 1 ) s
                                     ( opts, remainder ) = removel i options
                                 in case opts of
                                      [] -> ( (Nothing, []), s1 )
                                      (o:os) -> let (TextGen optf) = o
                                                    ( x, s2 ) = optf s1
                                                in ( ( Just x, remainder), s2 ) 


                                    
-- (list [ TextGen ]) -> a TextGen which does every option in order

list :: (RandomGen g) => [ TextGen g [ a ] ] -> TextGen g [ a ]
list []     = TextGen $ \s -> ( [], s )
list (o:os) = TextGen $ \s -> let (TextGen ofn) = o
                                  (TextGen osfn) = list os
                                  ( a, s' ) = ofn s
                                  ( as, ss' ) = osfn s'
                              in ( a ++ as, ss' )

-- (rep n TextgGen) -> a TextGen which repeats n times

rep :: (RandomGen g) => Int -> TextGen g [ a ] -> TextGen g [ a ]
rep n s = list $ take n $ repeat s

-- (randrep (min, max) TextGen) -> Repeat a Textgen (min, max) times

randrep :: (RandomGen g) => (Int, Int) -> TextGen g [ a ] -> TextGen g [ a ] 
randrep (min, max) s1 = TextGen $ \s -> let ( n, s' ) = randomR (min, max) s
                                            (TextGen s2f) = rep n s1
                                        in s2f s'
                                           
-- perhaps (n, m) TextGen -> Do a TextGen if rand(n) > m, otherwise empty

perhaps :: (RandomGen g) => (Int, Int) -> TextGen g [ a ] -> TextGen g [ a ]
perhaps (n, m) s1 = TextGen $ \s -> let ( n1, s' ) = randomR (0, m) s
                                        (TextGen s2f) = if n1 > n then list [] else s1
                                        in s2f s' 

-- loadOptions fileName -> loads a textfile and returns a choose TextGen

loadOptions :: (RandomGen g) => [Char] -> IO (TextGen g [[Char]])
loadOptions fname = do
  contents <- fmap T.lines (Tio.readFile fname)
  return $ choose $ map ( word . T.unpack ) contents

loadList :: (RandomGen g) => [Char] -> IO ([TextGen g [[Char]]])
loadList fname = do
  contents <- fmap T.lines (Tio.readFile fname)
  return $ map ( word . T.unpack ) contents


-- smartjoin: concatenates a list of strings with some attention to 
-- punctuation (ie commas get stuck to their left-hand neighbours without
-- spaces: terminal commas become full stops)



smartjoin :: [ [Char] ] -> [Char]
smartjoin ws = smartjoin_r $ undupe ws

undupe :: [[Char]] -> [[Char]]
undupe (w:x:ys) = if w == "," && x == "," then undupe(x:ys) else w:undupe(x:ys)
undupe (ws)     = ws

smartjoin_r :: [[Char]] -> [Char]
smartjoin_r (w:x:ys) = case x of
  "-" -> w ++ "-" ++ smartjoin_r(ys)
  "," -> case ys of
    [] -> w ++ "."
    otherwise -> w ++ ", " ++ smartjoin_r(ys)
  otherwise -> w ++ " " ++ smartjoin_r(x:ys)
smartjoin_r (w:xs) = case xs of
  [] -> w ++ "."
  otherwise -> w ++ " " ++ smartjoin_r(xs)
smartjoin_r [] = []

dumbjoin :: [ [ Char ] ] -> [ Char ]
dumbjoin s = intercalate " " s


upcase :: [Char] -> [Char]
upcase (x:xs) = (toUpper x):xs
upcase []     = []


