-- "Dao/Examples/FuzzyStrings.hs"  demonstrates how to create a simple pattern-matching rule.
-- 
-- Copyright (C) 2008-2015  Ramin Honary.
--
-- Dao is free software: you can redistribute it and/or modify it under the
-- terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
-- 
-- Dao is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
-- details.
-- 
-- You should have received a copy of the GNU General Public License along with
-- this program (see the file called "LICENSE"). If not, see the URL:
-- <http://www.gnu.org/licenses/agpl.html>.

module Dao.Examples.FuzzyStrings where

import           Dao
import           Dao.Text.PPrint
import qualified Dao.Tree as Tree

import           Control.Applicative
import           Control.Monad

import           Data.Char
import qualified Data.Text   as Strict
import           Data.Typeable

----------------------------------------------------------------------------------------------------

tokenize :: ToText t => t -> [[Strict.Text]]
tokenize = loop [] . fromText . toText where
  sp = dropWhile isSpace
  plus wx = if null wx then id else (++ [fmap Strict.pack wx])
  loop sents str = case breakup [] str of
    (wx, "" ) -> plus wx sents
    (wx, str) -> loop (plus wx sents) str
  breakup wx str = case sp str of
    ""                    -> (wx, "")
    '.':str               -> (wx, str)
    '-':c:str | isDigit c ->
      let (keep, more) = span isDigit str
      in  breakup (wx++['-':c:keep]) more
    c:str | isDigit c     ->
      let (keep, more) = span isDigit str
      in  breakup (wx++[c:keep]) more
    c:str | isAlpha c     ->
      let (keep, more) = span isAlpha str
      in  breakup (wx++[c:keep]) more
    _:str                 -> breakup wx str

----------------------------------------------------------------------------------------------------

newtype FuzzyString = FuzzyString { fuzzyStringText :: Strict.Text } deriving (Eq, Ord, Typeable)

instance Show FuzzyString where { show (FuzzyString str) = show str; }

instance ToText FuzzyString where { toText = fuzzyStringText; }

instance PPrintable FuzzyString where { pPrint = return . pText . fuzzyStringText; }

instance SimpleData FuzzyString where
  simple (FuzzyString str) = simple str
  fromSimple = fmap FuzzyString . fromSimple

instance ObjectPattern FuzzyString where
  objMatch (FuzzyString txt) o = case fromObj o of
    PTrue o -> case fuzzyCompare (Strict.map toLower txt) (Strict.map toLower o) of
      d | d<0.0 || d>1.0 -> error "fuzzyCompare evaluated to a meaningless value"
      d | d==1.0         -> ExactlyEqual
      d | d>0.67         -> Similar $ fromRational d
      _                  -> Dissimilar
    _    -> Dissimilar

instance ObjectData FuzzyString where
  obj fuzStr = obj
    $ printable fuzStr
    $ matchable fuzStr
    $ fromForeign fuzStr
  fromObj = fromObj >=> toForeign

fuzzyString :: ToText t => t -> FuzzyString
fuzzyString = FuzzyString . toText

fuzzyRule :: (Functor m, Applicative m, Monad m, ToText t) => t -> (Query -> Rule m a) -> Rule m a
fuzzyRule str = tree Tree.DepthFirst (fmap fuzzyString <$> tokenize str)

-- | Tokenizes the given text to a sequence of 'FuzzyString's and uses the sequence to construct a
-- 'Dao.Rule.Rule' that does nothing (just consumes the text). The 'Dao.Rule.Rule' is constructed
-- with 'Dao.Rule.Tree'
fuzzyText :: (Functor m, Applicative m, Monad m, ToText t) => t -> Rule m ()
fuzzyText str = fuzzyRule str $ const $ return ()

testSimilar :: String -> String -> IO ()
testSimilar a b = putStrLn $ unlines $ fmap unwords $
  [ ["fuzzy compare", show a, "and", show b++":"]
  , [show $ objMatch (obj $ fuzzyString a) (obj b)]
  ]

