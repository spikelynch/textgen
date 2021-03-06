{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TextGen (
  TextGen(..)
  ,TextGenCh(..)
  ,Vocab(..)
  ,runTextGen
  ,word
  ,tgempty
  ,aan
  ,postgen
  ,choose
  ,choose1
  ,choose1ple
  ,sampleR
  ,chooseN
  ,choose2
  ,choose3
  ,weighted
  ,remove
  ,list
  ,perhaps
  ,rep
  ,randrep
  ,smartjoin
  ,dumbjoin
  ,upcase
  ,loadList
  ,loadVocab
  ) where

import Control.Applicative
import Control.Monad (liftM, ap, forM_)
import Data.List (intercalate)
import Data.Char (toUpper)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import System.Random
import System.Directory (getDirectoryContents)
import Text.Regex.Posix


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

tgempty :: (RandomGen g) => TextGen g [[Char]]
tgempty = return [ ]




-- (choose [ TextGen ]) -> choose one of the TextGens in the list
                              


choose1 :: (RandomGen g) => TextGen g a -> [ TextGen g a ] -> TextGen g a
choose1 def  []   = def
choose1 _ options = TextGen $ \s -> let ( i, s' ) = randomR (0, (length options) - 1 ) s
                                        (TextGen optf) = options !! i
                                    in optf s'




weighted :: (RandomGen g) => [ ( Int, TextGen g a ) ] -> TextGen g a
weighted options = TextGen $ \s -> let ( option, s' ) = weightedR options s
                                       (TextGen optf) = option
                                   in optf s'

weightedR :: (RandomGen g) => [ ( Int, a ) ] -> g -> (a, g)
weightedR options g = let total = sum $ map (\(i, _) -> i) options
                          (i, g') = randomR ( 0, total - 1 ) g
                          a = selectR i options
                      in ( a, g' )

-- this breaks on an empty input list

selectR :: Int -> [ ( Int, a ) ] -> a
selectR i ((w, a):[]) = a
selectR i ((w, a):ws) = case i < w of
  True -> a
  False -> selectR ( i - w ) ws


-- for choose-and-remove: remember that a state combinator is a
-- lambda which wraps one or more existing combinators

-- so remove needs to -
--   - extract a random option from the list of options
--   - return that option and a new combinator with the new list
--   - to get the new combinator it has to call itself with the new list

removel :: Int -> [ a ] -> ( [ a ], [ a ] )
removel i l = let ( h, t ) = splitAt i l
          in case ( h, t ) of
               ( _, [] ) -> ( [], l )
               ( h, t:ts ) -> ( [ t ], h ++ ts )




-- Unlike choose, this is safe, because it returns Maybe A as the output

remove :: (RandomGen g) => [ TextGen g a ] -> TextGen g ( Maybe a, [ TextGen g a ] )
remove options = TextGen $ \s -> let ( i, s1 ) = randomR (0, (length options) - 1 ) s
                                     ( opts, remainder ) = removel i options
                                 in case opts of
                                      [] -> ( (Nothing, []), s1 )
                                      (o:os) -> let (TextGen optf) = o
                                                    ( x, s2 ) = optf s1
                                                in ( ( Just x, remainder), s2 ) 

-- chooseN (get multiple items from the same list without
-- repeats)

-- designing this differently from remove because I want it to run like
-- a "normal" combinator and throw away the remainder
-- It's still not exactly what I want, because it evaluates the combinators.
-- or is it?

-- Note after reflecting on this at tae kwon do grading:
--   it needs to return [ TextGen g a ] not TextGen g [ a ]
--   build a safe version of choose (a single item) and then
--   use that to make chooseN by recursion
--   flip the arguments: that way we can make a partial
--   function (choose [list]) which can be called with n, which is
--   a good way to adapt this to loading option files

-- Starting point: a safe version of choose

choose' :: (RandomGen g) => [ TextGen g a ] -> [ TextGen g a ]
choose' []     = []
choose' options = [ TextGen l ]
  where l = \s ->  let ( i, s1 )      = randomR (0, (length options) - 1) s
                       ( opts, _ )    = removel i options
                       (TextGen optf) = head opts
                       ( x, s2 )      = optf s1
                   in ( x, s2 )

-- A version which returns the rest of the options
-- I'm stuck: how does p2 jump out of the lambda l to become remainder?
-- Remember: don't need to rewrite remove, need to make a component which
-- can work as a recursive part of chooseN

remove' :: (RandomGen g) => [ TextGen g a ] -> TextGen g ( Maybe a,  [ TextGen g a ] )
remove' []     = TextGen $ \s -> ( (Nothing, []), s )
remove' options = TextGen l
  where l = \s ->  let ( i, s1 )      = randomR (0, (length options) - 1) s
                       ( p1, p2 )     = removel i options
                       (TextGen optf) = head p1
                       ( x, s2 )      = optf s1
                   in ( (Just x , options), s2 )

-- What does the lambda returned by remove' (and wrapped in a TextGen) do?
-- use the gen to run randomR to pick an option from the list
-- use the gen to run the option
-- return the result of the option, and the rest of the list, in a form
-- which can be unwrapped

-- now use remove' to make chooseN
-- order of arguments - list of options, then n of choices, so we
-- can make a partial function over a list

-- Separate the "pick n" from the "eval as TextGen" logic
-- write a recursive function to pick n random elements from a list
-- (of anything) and returns the new list, the remainder and a new gen
-- variable

-- Then write chooseN around it.
-- (possibly need to split the generator when it goes into the different
-- parts?)

-- getStdGen wants g -> ( a, g ) so (sampleR list 3)

sampleR :: (RandomGen g) => [ a ] -> Int -> g -> ( ([ a ], [ a ]), g )
sampleR xs 0 s = ( ([], xs), s )
sampleR xs n s = let ( i, s1 )      = randomR (0, (length xs) - 1) s
                     ( p1, rest )   = removel i xs
                     ( (p2, r2), s2 ) = sampleR rest (n - 1) s1
                 in ( (p1 ++ p2, r2), s2 )




-- take a list of generators and an integer, returns a list of n
-- generators, or fewer (if there were less than n in the original list)

chooseN :: (RandomGen g) => [ TextGen g a ] -> Int -> TextGen g [ TextGen g a ]
chooseN options n = TextGen $ \s -> let ( (sample, _), s1 ) = sampleR options n s                                  in ( sample, s1 )



list2onple :: (RandomGen g) => TextGen g [ TextGen g a ] -> TextGen g a -> TextGen g ( TextGen g a )
list2onple glist d = TextGen $ \s -> let (TextGen listf) = glist
                                         (results, s1) = listf s
                                         oneple = case results of
                                                    (a:[])    -> a 
                                                    otherwise -> d
                                     in ( oneple, s1 )

choose1ple :: (RandomGen g) => TextGen g a -> [ TextGen g a ] -> TextGen g ( TextGen g a )
choose1ple def options = list2onple (chooseN options 1) def


  
list2tuple :: (RandomGen g) => TextGen g [ TextGen g a ] -> TextGen g a -> TextGen g ( TextGen g a, TextGen g a )
list2tuple glist d = TextGen $ \s -> let (TextGen listf) = glist
                                         ( results, s1 ) = listf s
                                         tuple = case results of
                                                   (a:b:c) ->   ( a, b )
                                                   (a:[])  ->   ( a, d )
                                                   otherwise -> ( d, d )
                                     in ( tuple, s1 )



-- d is the default if the list doesn't have two elements

choose2 :: (RandomGen g) => TextGen g a -> [ TextGen g a ] -> TextGen g ( TextGen g a, TextGen g a )
choose2 def options = list2tuple (chooseN options 2) def


list3tuple :: (RandomGen g) => TextGen g [ TextGen g a ] -> TextGen g a -> TextGen g ( TextGen g a, TextGen g a, TextGen g a )
list3tuple glist d = TextGen $ \s -> let (TextGen listf) = glist
                                         ( results, s1 ) = listf s
                                         tuple = case results of
                                                   (a:b:c:d) ->   ( a, b, c )
                                                   (a:b:[])  ->   ( a, b, d )
                                                   (a:[])    ->   ( a, d, d )
                                                   otherwise ->   ( d, d, d )
                                     in ( tuple, s1 )


choose3 :: (RandomGen g) => TextGen g a -> [ TextGen g a ] -> TextGen g ( TextGen g a, TextGen g a, TextGen g a )
choose3 def options = list3tuple (chooseN options 3) def




                                       
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
                                           
-- perhaps (n, m) TextGen -> Do a TextGen if rand(m) > n, otherwise empty

perhaps :: (RandomGen g) => (Int, Int) -> TextGen g [ a ] -> TextGen g [ a ]
perhaps (n, m) s1 = TextGen $ \s -> let ( n1, s' ) = randomR (0, m) s
                                        (TextGen s2f) = if n1 > n then list [] else s1
                                        in s2f s' 

-- loadList fileName -> loads a textfile and returns a [ TextGen ]


loadList :: (RandomGen g) => [Char] -> IO ([TextGen g [[Char]]])
loadList fname = do
  contents <- fmap T.lines (Tio.readFile fname)
  return $ map ( word . T.unpack ) contents


-- generic transform: take a function f from [ a ] -> [ a ] and 'wrap' a
-- TextGen in it so the TextGen's output is applied to it before returning

postgen :: (RandomGen g) => ( [ a ] -> [ a ] ) -> TextGen g [ a ] ->  TextGen g [ a ]
postgen tr g = TextGen $ \s -> let (TextGen gf) = g
                                   ( orig, s' ) = gf s
                               in ( tr orig, s' )


aan :: (RandomGen g) => TextGen g [ [ Char ] ] -> TextGen g [ [ Char ] ]
aan g = postgen article g
  where article []       = []
        article (w:ws)   = (article_w w):w:ws
        article_w []     = []
        article_w (c:cs) = if vowel c then "an" else "a"

vowel :: Char -> Bool
vowel x = elem x "aeiouAEIOU"





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


-- snazzy context-sensitive choose
-- which doesn't work the way I want it to when it's sent to a
-- v "filename" (because this locks down the return type)

class TextChooser a where
  choose :: [ TextGenCh ] -> a 

type TGSimpleChooser   = TextGenCh
type TGOneChooser      = (TextGen StdGen TextGenCh)
type TGPairChooser     = (TextGen StdGen ( TextGenCh, TextGenCh ))
type TGTripleChooser   = (TextGen StdGen ( TextGenCh, TextGenCh, TextGenCh ))


instance TextChooser TGSimpleChooser where
  choose gs = choose1 (word "-") gs

instance TextChooser TGOneChooser where
  choose gs = choose1ple (word "-") gs

instance TextChooser TGPairChooser where
  choose gs = choose2 (word "-") gs

instance TextChooser TGTripleChooser where
  choose gs = choose3 (word "-") gs



-- Utilities for loading and manipulating vocab files

type TextGenCh = TextGen StdGen [[Char]]

type Vocab = (String -> [ TextGenCh ])


-- choice = choose $ v "vocabfile"
-- list = list $ v "vocabfile"
-- ( a, b ) <- choose2 $ v "vocabfile"

isTextFile :: String -> Bool
isTextFile f = f =~ ".txt$"


loadVocab :: String -> IO Vocab
loadVocab dir = do
  files <- getDirectoryContents dir
  list <- mapM loadFile $ filter isTextFile files
  return $ vocabGet $ Map.fromList list
    where loadFile f = do
            gens <- loadList ( dir ++ f )
            return ( f, gens )


vocabGet :: Map String [ TextGenCh ] -> String -> [ TextGenCh ]
vocabGet v name = case Map.lookup (name ++ ".txt") v of
  Nothing -> [ list $ map word [ "[",  "file not found:",  name, "]" ] ] 
  Just gens -> gens

getDir (x:xs) = x
getDir _      = "./"

