-- "Dao/Examples/FastNumbers.hs"  a program for converting numbers expressed in
-- english to numerical values.
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

-- | This program demonstrates how to create a simple natural language parsing program. The
-- documentation for this module is written as a tutorial which you can read from beginning to end.
-- Each function starts a new section of the tutorial.
--
-- We will create Dao production 'Dao.Rule.Rule's that can read a number written as a sentence in
-- the English language, and convert this number sentence to an 'Prelude.Integer' value, even if the
-- user makes minor spelling errors. For completeness, the inverse operation is also provided
-- (converting a 'Prelude.Integer' to an English language sentence).  To accomplish this, we make
-- use of the 'Dao.Examples.FuzzyStrings.FuzzyString' data type we defined in an earlier exercise.
--
-- Lets consider a strategy for how we can convert to and from English language number expressions
-- 'Prelude.Integer's. First we need a way to map words to number values. The "Data.Map" module
-- provided by the Haskell Platform's "containers" package will work well for this purpose.
--
-- In the English language, there are a few words to symbolize numbers:
--
-- * Simple counting words like "zero" through "nineteen" and words for multiples of ten like
--   "twenty", "thirty", "fourty", up through "ninety".
-- * Multipliers like "hundred", "thousand", "million", and "billion" and so on, which are applied
--   lower-valued words and summed to express arbitrary numbers.
--
-- Lets 'Data.Map.Map' both ways, lets 'Data.Map.Map' English words to 'Prelude.Integer' values, and
-- the inverse 'Data.Map.Map'ping 'Prelude.Integer's to English words. Then lets convert the
-- 'Data.Map.Map'pings from English words to 'Prelude.Integer's to 'Dao.Rule.Rule's, wrapping the
-- words in 'Dao.Examples.FuzzyStrings.FuzzyString's. Finally, we can program the grammar of the
-- production 'Dao.Rule.Rule's using ordinary monadic do notation, and the combinators provided in
-- "Control.Monad" and "Control.Applicative" and "Dao.Rule", as we would with any computer language
-- parser.
--
-- To run this program in GHCi, navigate to the top level of the "Dao-examples" package and launch
-- GHCi like so:
--
-- > ghci -i./src Dao.Examples.Numbers
--
-- Then run the "main" function in the "Dao.Examples.Numbers" module:
--
-- > main
--
-- This will enter into a Read-Eval-Print loop managed by Readline.
module Dao.Examples.Numbers
  ( -- * Preliminaries
    testRule, NumberRule,
    -- * Programming the Spelling of Numbers
    singleDigitsMap, teensMap, doubleDigitsMap, zillionsMap,
    -- * Converting 'Data.Map.Map's to Dao Production 'Dao.Rule.Rule's.
    ruleFromMap, numberDigits, 
    -- * The Fundamental Combinators
    singleDigits, teens, doubleDigits, zillions,
    -- * Programming English Language Semantics
    subHundred, _hundred, _and, hundreds, numberSentence,
    -- * The Final Product
    number, digitsToWords, main
  )
  where

import           Dao.Examples.FuzzyStrings
import           Dao.Object
import           Dao.Rule
import           Dao.Text
import qualified Dao.Tree  as T

import           Data.Either
import           Data.Functor.Identity
import           Data.Monoid
import qualified Data.Map  as M
import qualified Data.Text as Strict

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except

import           System.Console.Readline

-- | The 'testRule' function can be used to run any of the 'NumberRule' functions defined in this
-- module. You can use it in @GHCi@ to test any one of the example parsers defined here to get a
-- feel of how it works.
testRule :: Rule Identity o -> String -> [Either ErrorObject o]
testRule rule = tokenize >=> runIdentity . queryAll rule . fmap obj

----------------------------------------------------------------------------------------------------

-- | 'NumberRule' will be the type we use for all of our Dao production 'Dao.Rule.Rule's.
--
-- It is a good idea to define 'Dao.Rule.Rule's to be polymorphic over the monadic type.
-- Unfortunately, this means always writing three classes into the context of the function:
-- 'Data.Functor.Functor', 'Control.Applicative.Applicative', and 'Control.Monad.Monad'. If you want
-- to use 'Control.Monad.IO.Class.liftIO' in your 'Dao.Rule.Rule's, you also need to provide
-- 'Control.Monad.IO.Class.MonadIO' in the context.
--
-- We can reduce the amount of typing we need to do by enabling the GHC language extension
-- @RankNTypes@, and creating a data type like 'kNumberRule' where the context is specified.
type NumberRule m i = forall m . (Functor m, Applicative m, Monad m) => Rule m i

-- | The 'singleDigitsMap' is a 'Data.Map.Map'ing from an 'Prelude.Integer' to the correct (and some
-- incorrect) spellings of the English language word for that 'Prelude.Integer'.
--
-- @
-- singleDigitsMap = 'Data.Map.fromList' $ 
--   [ (0, "zero no"),
--     (1, "one a on won wun"),
--     (2, "two to too tu tuu"),
--     (3, "three free"),
--     (4, "four"),
--     (5, "five"),
--     (6, "six si ix"),
--     (7, "seven"),
--     (8, "eight ate"),
--     (9, "nine")
--   ]
-- @
singleDigitsMap :: M.Map Integer String
singleDigitsMap = M.fromList $ 
  [ (0, "zero no"),
    (1, "one a on won wun"),
    (2, "two to too tu tuu"),
    (3, "three free"),
    (4, "four"),
    (5, "five"),
    (6, "six si ix"),
    (7, "seven"),
    (8, "eight ate"),
    (9, "nine")
  ]

-- | The 'teensMap' is for numbers between 10 and 19. In English, numbers betwen 10 and 19 have
-- their own special words, so we specify those words here.
--
-- @
-- teensMap = 'Data.Map.fromList' $
--   [ (10, "ten tn te"),
--     (11, "eleven"),
--     (12, "twelve"),
--     (13, "thirteen"),
--     (14, "fourteen"),
--     (15, "fifteen"),
--     (16, "sixteen"),
--     (17, "seventeen"),
--     (18, "eighteen"),
--     (19, "nineteen")
--   ]
-- @
teensMap :: M.Map Integer String
teensMap = M.fromList $
  [ (10, "ten tn te"),
    (11, "eleven"),
    (12, "twelve"),
    (13, "thirteen"),
    (14, "fourteen"),
    (15, "fifteen"),
    (16, "sixteen"),
    (17, "seventeen"),
    (18, "eighteen"),
    (19, "nineteen")
  ]

-- | The 'doubleDigitsMap' provides a 'Data.Map.Map'ping for multiples of ten between 10 an 100,
-- which in the English language, all have their own special words, so we specify those words here.
-- 
-- @
-- doubleDigitsMap = M.fromList $
--   [ (20, "twenty"),
--     (30, "thirty"),
--     (40, "fourty"),
--     (50, "fifty"),
--     (60, "sixty"),
--     (70, "seventy"),
--     (80, "eighty"),
--     (90, "ninety")
--   ]
-- @
doubleDigitsMap :: M.Map Integer String
doubleDigitsMap = M.fromList $
  [ (20, "twenty"),
    (30, "thirty"),
    (40, "fourty"),
    (50, "fifty"),
    (60, "sixty"),
    (70, "seventy"),
    (80, "eighty"),
    (90, "ninety")
  ]

-- | 'zillionsMap' is a 'Data.Map.Map'ping from 'Prelude.Integer' values to strings for the numbers
-- for exponents of a "thousand", i.e. every number ending in "...illion", for as many numbers that
-- I know of. To make it easier to write, I use 'Data.Functor.fmap' to append the string "illion" to
-- all of the words except for the word "thousand".
--
-- @
-- zillionsMap = M.fromList $ 'Prelude.zip' ('Prelude.iterate' (1000 *) 1000) $
--   ("thousand" :) $ 'Data.Functor.fmap' (++ "illion") $
--     [ "m", "b", "tr", "quadr", "quint", "sext", "sept", "oct", "non", "dec", "undec", "dodec",
--       "tredec", "quattuordec", "quinquadec", "sexdec", "septdec", "octdec", "novendec", "vigint"
--     ]
-- @
zillionsMap :: M.Map Integer String
zillionsMap = M.fromList $ zip (iterate (1000 *) 1000) $
  ("thousand" :) $ fmap (++ "illion") $
    [ "m", "b", "tr" , "quadr", "quint", "sext" , "sept", "oct", "non" , "dec", "undec", "dodec"
    , "tredec", "quattuordec", "quinquadec" , "sexdec", "septdec", "octdec" , "novendec", "vigint"
    ]

-- | 'ruleFromMap' will convert the above 'Data.Map.Map's to Dao a production 'Dao.Rule.Rule'. First
-- we need to convert the 'Data.Map.Map's to a list of associations (pairs) using 'Dao.Map.assocs'.
--
-- The format of the 'Prelude.String' elements of the 'Data.Map.Map's above are simple, each is a
-- string of space-separated words providing alternative spellings for the 'Prelude.Integer' key it
-- is associated with.
--
-- Our 'Dao.Examples.FuzzyStrings.FuzzyString's are good at matching misspelled words, but some
-- alternative spellings of words, which sound similar but are spelled differently, may be
-- considered too different for the 'Dao.Examples.FuzzyStrings.FuzzyString' to match, for example
-- writing "tu" instead of "two". It would be convenient if our intelligent program could
-- understand that when a user writes "tu" they mean to say "two", but these two spellings are too
-- different for 'Dao.Examples.FuzzyStrings.FuzzyString's to be of use. So lets explicitly declare
-- that both spellings map to the same 'Prelude.Integer' value of @2@.
--
-- Then we can use 'Prelude.words' to break up each word into a list of alternative spellings. Then
-- we use 'Data.Functor.fmap' to convert each association to a 'Dao.Rule.Rule' monadic function that
-- returns an 'Prelude.Integer' value. Every alternative spelling is assigned to the same
-- 'Prelude.Integer' value. 
--
-- Finally, we use 'Control.Monad.msum' to combine every generated 'Dao.Rule.Rule' into a single
-- 'Dao.Rule.Rule' that can behave as any one of the 'Dao.Rule.Rule's generated from each
-- association.
--
-- The most important part of this function is that we use the 'Dao.Rule.tree' function to create
-- our production 'Dao.Rule.Rule's. The 'Dao.Rule.tree' function takes three parameters:
--
-- 1. a 'Dao.Tree.RunTree' control parameter which specifies whether the 'Dao.Tree.Tree' inside of
--    the 'Dao.Rule.Rule' should be matched in 'Dao.Tree.DepthFirst' or 'Dao.Tree.BreadthFirst'
--    order. Since we always want the longest possible match to succeed, we choose
--    'Dao.Tree.DepthFirst' order, although in this case it doesn't really matter as every branch of
--    our tree has only one word, and so every branch has a depth of 1.
-- 2. a list of tree branches. The branches of the tree are a list of branch segments, where each
--    segment acts as a pattern that can match an item in a input 'Dao.Rule.Query'. Each branch
--    segment may be of any data type that instantiates 'Dao.Object.ObjectData'. In this case we
--    want to convert each word to a 'Dao.Examples.FuzzyStrings.FuzzyString'.
-- 3. an action function to be evaluated when a portion of an input 'Dao.Rule.Query' that has matched
--    the current 'Dao.Tree.Tree' branch. This function must take the portion of the
--    'Dao.Rule.Query' that matched the pattern provided in parameter (2), and convert this to a
--    useful value, for example the 'Prelude.Integer' value. In our case, we do not care about the
--    words in the input 'Dao.Rule.Query' that have matched, the action is only evaluated if the
--    input 'Dao.Rule.Query' matches the pattern so the fact that the function is evaluated means
--    the end users has input the number word we were expecting. So all we need to do is return the
--    associated 'Prelude.Integer' value. Thus we will use 'Prelude.const' to discard the input
--    parameters and 'Control.Monad.return' the 'Prelude.Integer' key from the 'Data.Map.Map'.
--
-- @
-- ruleFromMap = 'Control.Monad.msum' . 'Data.Functor.fmap' convertAssocToRule . 'Data.Map.assocs' where
--   convertAssocToRule (i, str) =
--     'Dao.Rule.tree' 'Dao.Tree.DepthFirst'
--          ('Data.Functor.fmap' (\\str -> ['Dao.Examples.FuzzyStrings.fuzzyString' str]) $ 'Prelude.words' str)
--          ('Prelude.const' $ return i)
-- @
--
-- Notice in the code
--
-- @
-- ('Data.Functor.fmap (\\str -> ['Dao.Examples.FuzzyStrings.fuzzyString' str]) $ 'Prelude.words' str)
-- @
--
-- Each branch is an alternative spelling for a word, and contains just one
-- 'Dao.Examples.FuzzyStrings.FuzzyString' pattern. If we wanted to create branches with multiple
-- words, we could do something like this:
--
-- @
-- ('Data.Functor.fmap (\\str -> ['Data.Functor.fmap' 'Dao.Examples.FuzzyStrings.fuzzyString' $ 'Prelude.words' str]))
-- @
ruleFromMap :: M.Map Integer String -> NumberRule m Integer
ruleFromMap = msum . fmap convertAssocToRule . M.assocs where
  convertAssocToRule (i, str) =
    tree T.DepthFirst
         (fmap (\str -> [fuzzyString str]) $ words str)
         (const $ return i)

-- | The 'numberDigits' 'Dao.Rule.Rule' will parse an ordinary number expressed as a string of
-- digits.
--
-- With the 'ruleFromMap' function we defined a way to create Dao production 'Dao.Rule.Rule's out of
-- strings. But our program will be parsing input typed by a human, so lets also consider the case
-- where a user has entered a numerical value expressed as a string of base-10 digits "0123456789".
--
-- Where the 'Dao.Rule.tree' function created rules from 'Dao.Examples.FuzzyStrings.FuzzyString'
-- 'Dao.Object.Object's, we can also create 'Dao.Rule.Rule's that match an 'Dao.Object.Object' of a
-- specific Haskell data type. This can be done with the 'Dao.Rule.infer' combinator for any Haskell
-- data type instantiating 'Data.Typeable.Typeable'. As a reminder, you can instantiate your own
-- custom data types into the 'Data.Typeable.Typeable' class by using the GHC language extension
-- @DeriveDataTypeable@ and including a @deriving 'Data.Typeable.Typeable'@ clause after the
-- definition of your Haskell @data@ type in your source code.
--
-- The 'Dao.Rule.infer' function will infer the data type on which you intend to pattern match by
-- using 'Data.Typeable.typeOf' on the 'Dao.Rule.Rule' function that you pass to it as an argument.
-- For example, if you pass a 'Dao.Rule.Rule' function of type:
--
-- @
-- (('Data.Functor.Functor' m, 'Control.Applicative.Applicative' m, 'Control.Monad.Monad' m) => 'Data.Text.Text' -> 'Dao.Rule.Rule' m ()
-- @
--
-- The 'Dao.Rule.infer' function will create an 'Dao.Object.Object' pattern that matches any
-- 'Data.Text.Text' 'Dao.Object.Object' in the input 'Dao.Rule.Query'. The 'Data.Typeable.TypeRep'
-- value is actually wrapped in a 'Dao.Object.Object' and stored into the 'Dao.Tree.Tree' of
-- patterns inside of the Dao production 'Dao.Rule.Rule'.
--
-- So lets create a Dao production 'Dao.Rule.Rule' that matches an 'Data.Text.Text'
-- 'Dao.Object.Object' in the input 'Dao.Rule.Query', then tries to parse this 'Data.Text.Text' as a
-- 'Prelude.Integer' value using the Haskell function 'Text.Read.readsPrec'. If the parse is
-- successful, the 'Dao.Rule.Rule' should succeed and return the 'Prelude.Integer' value. If the
-- parse fails, the 'Dao.Rule.Rule' should fail with 'Control.Monad.mzero' or
-- 'Control.Applicative.empty'.
--
-- @
-- numberDigits = 'Dao.Rule.infer' $ \\str -> case 'Text.Read.readsPrec' 0 $ 'Data.Text.unpack' str of
--    [(i, "")] -> return i
--    _         -> 'Control.Monad.mzero'
-- @
numberDigits :: NumberRule m Integer
numberDigits = infer $ \str -> case readsPrec 0 $ Strict.unpack str of
  [(i, "")] -> return i
  _ -> mzero

-- | This 'Dao.Rule.Rule' is simply defined as: @'ruleFromMap' 'singleDigitsMap'@
singleDigits :: NumberRule m Integer
singleDigits = ruleFromMap singleDigitsMap

-- | This 'Dao.Rule.Rule' is simply defined as: @'ruleFromMap' 'teensMap'@
teens :: NumberRule m Integer
teens = ruleFromMap teensMap

-- | This 'Dao.Rule.Rule' is simply defined as: @'ruleFromMap' 'doubleDigitsMap'@
doubleDigits :: NumberRule m Integer
doubleDigits = ruleFromMap doubleDigitsMap

-- | This 'Dao.Rule.Rule' is simply defined as: @'ruleFromMap' 'zilionsMap'@
zillions :: NumberRule m Integer
zillions = ruleFromMap zillionsMap

-- | 'subHundred' combines the simpler combinators 'singleDigits', 'teens', and 'doubleDigits', to
-- create a parser that can parse English words for numbers less than one hundred. This will be our
-- first complex combinator. Here is the equation for it:
--
-- @
-- ('Prelude.+') 'Control.Applicative.<$>' 'doubleDigits' 'Control.Applicative.<*>' ('Prelude.maybe' 0 'Prelude.id' 'Control.Applicative.<$>' 'Control.Applicative.optional' 'singleDigits'),
-- @
--
-- Here we make use of the 'Control.Applicative.Applicative' operator @('Control.Applicative.<*>')@
-- to apply a 'doubleDigits' 'Prelude.Integer' value and an 'Control.Applicative.optional'
-- 'singleDigits' 'Prelude.Integer' value to the sum function @('Prelude.+')@.
-- 'Control.Applicative.Applicative' functions always evaluate in order from left to right, so our
-- 'Dao.Rule.Rule's will match in sequence. The sum function is pure and needs to be lifted to the
-- 'Dao.Rule.Rule' monad, which we can do with the 'Data.Functor.Functor' operator
-- @('Control.Applicative.<$>')@.
--
-- What this means is, if we see a 'doubleDigits' number like "twenty" or "fifty", and it is
-- optionally followed by a 'singleDigits' number like "four" or "nine", then add these numbers
-- together: "twenty four" evaluates to @20 + 4@, "fifty nine" evaluates to @50 + 9@. We can use the
-- 'Prelude.maybe' function to indicate that if the 'singleDigits' value was not specified, we
-- should just add zero: "eigthy" evaluates to @80 + 0@.
--
-- It is also valid to express 'singleDigits' or 'teens' alone. So lets specify two more
-- 'Control.Applicative.Alternative' expressions. Lets use 'Control.Monad.msum' to list out all the
-- alternative ways an English speaker might express a number less than one hundred. We could also
-- use the 'Control.Applicative.Alternative's operator @('Control.Applicative.<|>')@ to list each
-- alternative, but I prefer list syntax myself.
--
-- @
-- subHundred = 'Control.Monad.msum' $
--   [ ('Prelude.+') 'Control.Applicative.<$>' 'doubleDigits' 'Control.Applicative.<*>' ('Prelude.maybe' 0 'Prelude.id' 'Control.Applicative.<$>' 'Control.Applicative.optional' 'singleDigits'),
--     'teens',
--     'singleDigits'
--   ]
-- @
--
-- And that is all we need to do to match any expression less than 100. Well that was easy!
subHundred :: NumberRule m Integer
subHundred = msum $
  [ (+) <$> doubleDigits <*> (maybe 0 id <$> optional singleDigits),
    teens,
    singleDigits
  ]

-- | '_hundred' is a trivial combinator that only matches the word "hundred".
--
-- @
-- _hundred = 'Dao.Examples.FuzzyStrings.fuzzyText' "hundred" 'Control.Monad.>>' return 100
-- @
_hundred :: NumberRule m Integer
_hundred = fuzzyText "hundred" >> return 100

-- | '_and' is a trivial combinator for the word "and" which can appear in various places in the
-- grammar of an English language number expression. Lets also include multiple possible spellings
-- for it. Lets also make it a completely optional word by including a @return ()@ as the final
-- choice, i.e. if none of the spellings match, we return successfully anyway.
--
-- @
-- _and = 'Control.Monad.msum' $ 'Data.Functor.fmap' 'Dao.Examples.FuzzyStrings.fuzzyText' ('Prelude.words' "and und an nd n") 'Prelude.++' [return ()]
-- @
_and :: NumberRule m ()
_and = msum $ fmap fuzzyText (words "and und an nd n") ++ [return ()]

-- | The 'hundreds' function will match any numerical expression less than 1000.
--
-- The word "hundred" can be used alone or succeeding some 'singleDigits'. As we saw with the
-- 'subHundred' example, we can use 'Control.Applicative.Applicative' operators to construct
-- 'Dao.Rule.Rule's that match sequences, and we can use the 'Control.Applicative.Alternative'
-- operator and 'Control.Monad.msum' to describe alternative ways of expressing numbers. Lets
-- continue building on what we have done before.
-- 
-- First lets think of an example expression that we could parse. The words "nine hundred and eighty
-- seven":
--
-- * "nine" -- this can be matched by 'singleDigits', which would return the 'Prelude.Integer' @9@.
-- * "hundred and" -- this can be matched by sequencing '_hundred' and '_and'
-- * "eighty seven" -- this can be matched by 'subHundred', which would return the 'Prelude.Integer' @87@.
--
-- The semantics of the words "nine hundred and eighty seven" map to the equation @(9*100 + 87)@,
-- and we can generalize this to a function @(\\a b -> a*100 + b)@ for any 'singleDigits' @a@ and any
-- 'subHundred' value @b@.
--
-- We will also need to match the words "hundred and" in between matching of the 'singleDigits' and
-- 'subHundred's, which can be done with the '_hundred' 'Dao.Rule.Rule'. The 'Dao.Rule.Rule' for
-- '_and' was defined to be optional, and returns a value of @()@ so we can combined it with the
-- '_hundred' function. So lets modify our equation to accept the value returned by '_hundred':
-- @(\\a hundred b -> a*hundred + b)@.
--
-- It is now obvious how to define our 'Dao.Rule.Rule':
--
-- @
-- (\\a hundred b -> a*hundred + b) 'Control.Applicative.<$>' 'singleDigits' 'Control.Applicative.<*>' (_hundred 'Control.Applicative.<*' _and) 'Control.Applicative.<*>' 'subHundred'
-- @
--
-- An educated Haskell programmer will recall that the @('Control.Applicative.<*')@ operator
-- evaluates both functions in order, but returns only the value from the left, so we can parse the
-- word "and" but the expression will only evaluate to the result parsed by '_hundred'.
--
-- Speakers of English also say things like "nineteen hundred", so lets include a rule for this
-- possibility. The equation for this rule is simlpy to multiply the words for 19 and the word for
-- 100. So our equation is: @(19*100)@ which generalizes to @(\\a b -> a*b)@ which in Haskell can be
-- written simply as @(*)@. So our next rule is:
-- 
-- @
-- (*) 'Control.Applicative.<$>' 'singleDigits' 'Control.Applicative.<*>' '_hundred'
-- @
--
-- Lets also suppose the end user writes their number as a string of digits and also include the
-- 'numberDigits' 'Dao.Rule.Rule', but for the sake of consistency, lets limit this rule to only
-- numbers less than 1000. After all, the name of this function is 'hundreds' and we wouldn't want
-- to confuse the 'numberSentence' 'Dao.Rule.Rule' which uses this 'hundreds' 'Dao.Rule.Rule', and
-- expects 'hundreds' to always return a value less than 1000 -- we don't want to accept input like
-- "48275 hundred and one".
--
-- @
-- 'numberDigits' >>= \\i -> 'Control.Monad.guard' (i\<1000) >> return i
-- @
--
-- Finally, lets combine all of the 'Dao.Rule.Rule's we have written so far so that our 'hundreds'
-- rule can parse any number expression less than 1000. Again I use 'Control.Monad.msum' to combine
-- each 'Control.Applicative.Alternative' because I prefer list notation, but is equivalent to use
-- the 'Control.Applicative.Alternative' operator @('Control.Applicative.<|>')@.
--
-- @
-- hundreds = 'Dao.Rule.bestMatch' $ 'Control.Monad.msum' $
--   [ (\\a hundred b -> a*hundred + b) 'Control.Applicative.<$>' 'singleDigits' <*> ('_hundred' 'Control.Applicative.<*' '_and') 'Control.Applicative.<*>' 'subHundred',
--     (*) 'Control.Applicative.<$>' 'subHundred' 'Control.Applicative.<*>' '_hundred',
--     'subHundred', '_hundred',
--     'numberDigits' >>= \\i -> 'Control.Monad.guard' (i\<1000) >> return i
--   ]
-- @
--
-- The final thing to note here is the use of the 'Dao.Rule.bestMatch' function. When a
-- 'Dao.Rule.Rule' is matching against a 'Dao.Rule.Query' input, all possible branches are evaluated
-- in parallel (I mean /logically/  in parallel, not in separate threads). So for example, the input
-- 'Dao.Rule.Query' for "two hundred and thirteen" will have the rule 'subHundred' match in parallel
-- with the rule for @(\\a hundred b -> a*hundred + b)@. As evaluation continues, this will create
-- two parallel branches of evaluation, one 'subHundred' matches the word "two", and one where
-- 'singleDigits' matches the word "two". All future evaluation must try both branches, which can be
-- computationally expensive. Rules that create too many possibilities will grow exponentially in
-- complexity.
--
-- To mitigate this problem, the 'Dao.Rule.bestMatch' function is provided in the "Dao.Rule" module.
-- This function forces evaluation of it's given 'Dao.Rule.Rule' function, eliminating laziness, but
-- also eliminating all branches of evaluation except for the one which matched the most
-- 'Dao.Rule.Query' input. Said another way, 'Dao.Rule.bestMatch' forces evaluation to be
-- 'Dao.Tree.DepthFirst' and takes only the longest branch of evaluation, eliminating all other
-- possible branches before proceeding.
hundreds :: NumberRule m Integer
hundreds = bestMatch $ msum $
  [ (\a hundred b -> a*hundred + b) <$> singleDigits <*> (_hundred <* _and) <*> subHundred,
    (*) <$> subHundred <*> _hundred,
    subHundred, _hundred,
    numberDigits >>= \i -> guard (i<1000) >> return i
  ]

-- | 'numberSentence' will match any number expressed as an english language sentence.
--
-- Numbers are any sequence of a 'hundreds' expression followed by a 'zillions' expression. Of
-- course, convention dictates the speaker should order the sequence from highest to lowest
-- significance, but we do not need to care about the ordering, as long as there is no ambiguity.
--
-- The simplest way to do this function would be to do a simple recursion:
--
-- @
-- numberSentence n = do
--     h <- 'hundreds'
--     'Control.Monad.msum' $
--       [ (\\z n -> h\*z + n) 'Control.Applicative.<$>' 'zillions' 'Control.Applicative.<*>' 'numberSentence',
--         return (h+n)
--       ]
-- @
--
-- But we would probably like to inform the user of ambiguity if they should accidentally say the
-- word "million" twice, they may after all have meant to say "billion" and we would like to be
-- sure. So we create a 'Data.Map.Map' that keeps track of which 'zillions' numbers have already
-- been said and use 'Control.Monad.Errorl.throwError' to inform the end users of the ambiguity.
-- Actually we can use 'Dao.Object.throwObject' to throw an 'Dao.Object.Object', so we can make use
-- of Dao's dynamic typing and not have to worry about the statically typed
-- 'Control.Monad.Error.Class.MonadError' type.
--
-- @
-- numberSentence = 'Dao.Rule.bestMatch' $ loop 'Data.Monoid.mempty' 0 where
--   loop saidAlready n = do
--     h <- 'hundreds'
--     'Control.Monad.msum' $
--       [( do z <- 'zillions' <* _and
--             case 'Data.Map.lookup' z saidAlready of
--               'Prelude.Nothing' -> let i = n+h\*z in return i 'Control.Applicative.<|>' loop ('Data.Map.insert' z () saidAlready) i
--               'Prelude.Just' () -> 'Dao.Object.throwObject' $ 'Prelude.concat' $
--                 [ "You said ", maybe "something-zillion" 'Prelude.show' $ 'Data.Map.lookup' z 'zillionsMap',
--                   " more than once."
--                 ]
--         ),
--         return (h+n)
--       ]
-- @
numberSentence :: NumberRule m Integer
numberSentence = bestMatch $ loop mempty 0 where
  loop saidAlready n = do
    h <- hundreds
    msum $
      [( do z <- zillions <* _and
            case M.lookup z saidAlready of
              Nothing -> loop (M.insert z () saidAlready) (h*z + n)
              Just () -> throwObject $ concat $
                [ "You said ", maybe "something-zillion" show $ M.lookup z zillionsMap,
                  " more than once."
                ]
        ),
        return (h+n)
      ]

----------------------------------------------------------------------------------------------------

-- | This function converts a 'Prelude.Integer' to a sequence of 'Data.Text.Text' words describing
-- the 'Prelude.Integer' as an English language sentence.
--
-- This is not an interesting or difficult problem at all, but what intelligent program wouldn't
-- allow you to do this? It is included here for good measure. Although it is limited to numbers
-- less thant @10^66 - 1@ because the 'zillionsMap' only has enough words to describe numbers up to
-- that point.
digitsToWords :: Integer -> [Strict.Text]
digitsToWords i = toText <$> if i==0 then ["zero"] else isNeg $ convert 1 $ breakup $ abs i where
  isNeg        = if i<0 then ("negative" :) else id
  breakup   i  = if i==0 then [] else uncurry (\a b -> b : breakup a) $ divMod i 1000
  convert z ix = case ix of
    []   -> []
    0:ix -> convert (1000 * (z::Integer)) ix
    i:ix -> do
      let (hundreds, dd) = divMod i 100
      let (tens,   ones) = divMod dd 10
      let lookup i m = if i==0 then Just [] else take 1 . words <$> M.lookup i m
      mplus (convert (1000*z) ix) $ maybe ["MY BRAIN HURTS!!!"] id $
        if i==100 then Just $ words "one hundred" else do
          hundreds <- (>>= (: (words "hundred and"))) <$> lookup hundreds singleDigitsMap
          tens <- mplus (lookup dd teensMap) $
            (++) <$> lookup (10*tens) doubleDigitsMap <*> lookup ones singleDigitsMap
          illion <- if z==1 then Just [] else M.lookup z zillionsMap
          Just $ hundreds ++ tens ++ [illion]

----------------------------------------------------------------------------------------------------

-- | This is the one 'Dao.Rule.Rule' to rule them all: the 'number' rule. This 'Dao.Rule.Rule'
-- combines the 'numberSentence' 'Dao.Rule.Rule' and the 'numberDigits' rule, so an end user can
-- enter a string of digits or a sentence of words expressing a number, and this 'Dao.Rule.Rule'
-- will match the input 'Dao.Rule.Query' and do the right thing.
number :: NumberRule m Object
number = bestMatch $
  (obj . Strict.unwords . digitsToWords <$> numberDigits) <|> (obj <$> numberSentence)

-- | This function will enter into a Read-Eval-Print Loop (REPL) controlled by "readline". You can
-- enter any number and it will be 'Dao.Examples.FuzzyStrings.tokenize'd, each token will be
-- converted to a list of 'Dao.Object.Object's, and this list of 'Dao.Object.Object's (also known as
-- a 'Dao.Rule.Query') will be fed into the 'number' production 'Dao.Rule.Rule'. Exit REPL with
-- Control-D, or whatever you have configured for your exit key in the "readline" configuration.
--
-- For your convenience, here is the full source of this module, cut-and-pasted right into this
-- documentation. Specifying all production 'Dao.Rule.Rule's only requres only 112 lines of code.
--
-- @
-- module "Dao.Examples.Numbers" where
-- 
-- import           "Dao.Examples.FuzzyStrings"
-- import           "Dao.Object"
-- import           "Dao.Rule"
-- import           "Dao.Text"
-- import qualified "Dao.Tree"  as T
-- 
-- import           "Data.Either"
-- import           "Data.Functor.Identity"
-- import           "Data.Monoid"
-- import qualified "Data.Map"  as M
-- import qualified "Data.Text" as Strict
-- 
-- import           "Control.Applicative"
-- import           "Control.Monad"
-- import           "Control.Monad.Except"
-- 
-- import           "System.Console.Readline"
-- 
-- 'testRule' :: 'Dao.Rule.Rule' 'Control.Monad.Identity.Identity' o -> 'Prelude.String' -> ['Prelude.Either' 'Dao.Object.ErrorObject' o]
-- 'testRule' rule = 'Dao.Examples.FuzzyString.tokenize' 'Control.Monad.>=>' 'Control.Monad.Identity.runIdentity' . 'Dao.Rule.queryAll' rule . 'Data.Functor.fmap' 'Dao.Object.obj'
-- 
-- type 'NumberRule' m i = forall m . ('Data.Functor.Functor' m, 'Control.Applicative.Applicative' m, 'Control.Monad.Monad' m) => 'Dao.Rule.Rule' m i
-- 
-- 'singleDigitsMap' :: M.'Data.Map.Map' 'Prelude.Integer' 'Prelude.String'
-- 'singleDigitsMap' = M.'Data.Map.fromList' $ 
--   [ (0, "zero no"),
--     (1, "one a on won wun"),
--     (2, "two to too tu tuu"),
--     (3, "three free"),
--     (4, "four"),
--     (5, "five"),
--     (6, "six si ix"),
--     (7, "seven"),
--     (8, "eight ate"),
--     (9, "nine")
--   ]
-- 
-- 'teensMap' :: M.'Data.Map.Map' 'Prelude.Integer' 'Prelude.String'
-- 'teensMap' = M.'Data.Map.fromList' $
--   [ (10, "ten tn te"),
--     (11, "eleven"),
--     (12, "twelve"),
--     (13, "thirteen"),
--     (14, "fourteen"),
--     (15, "fifteen"),
--     (16, "sixteen"),
--     (17, "seventeen"),
--     (18, "eighteen"),
--     (19, "nineteen")
--   ]
-- 
-- 'doubleDigitsMap' :: M.'Data.Map.Map' 'Prelude.Integer' 'Prelude.String'
-- 'doubleDigitsMap' = M.'Data.Map.fromList' $
--   [ (20, "twenty"),
--     (30, "thirty"),
--     (40, "fourty"),
--     (50, "fifty"),
--     (60, "sixty"),
--     (70, "seventy"),
--     (80, "eighty"),
--     (90, "ninety")
--   ]
-- 
-- 'zillionsMap' :: M.'Data.Map.Map' 'Prelude.Integer' 'Prelude.String'
-- 'zillionsMap' = M.'Data.Map.fromList' $ 'Prelude.zip' ('Prelude.iterate' (1000 *) 1000) $
--   ("thousand" :) $ 'Data.Functor.fmap' ('Prelude.++' "illion") $
--     [ "m", "b", "tr" , "quadr", "quint", "sext" , "sept", "oct", "non" , "dec", "undec", "dodec"
--     , "tredec", "quattuordec", "quinquadec" , "sexdec", "septdec", "octdec" , "novendec", "vigint"
--     ]
-- 
-- 'ruleFromMap' :: M.'Data.Map.Map' 'Prelude.Integer' 'Prelude.String' -> NumberRule m 'Prelude.Integer'
-- 'ruleFromMap' = 'Control.Monad.msum' . 'Data.Functor.fmap' convertAssocToRule . M.'Data.Map.assocs' where
--   convertAssocToRule (i, str) =
--     'Dao.Rule.tree' T.'Dao.Tree.DepthFirst'
--          ('Data.Functor.fmap' (\\str -> ['Dao.Examples.FuzzyStrings.fuzzyString' str]) $ 'Prelude.words' str)
--          ('Prelude.const' $ 'Control.Monad.return' i)
-- 
-- 'numberDigits' :: 'NumberRule' m 'Prelude.Integer'
-- 'numberDigits' = 'Dao.Rule.infer' $ \\str -> case 'Text.Read.readsPrec' 0 $ Strict.'Data.Text.unpack' str of
--   [(i, "")] -> 'Control.Monad.return' i
--   _         -> 'Control.Monad.mzero'
-- 
-- 'singleDigits' :: 'NumberRule' m 'Prelude.Integer'
-- 'singleDigits' = 'Prelude.ruleFromMap' 'Prelude.singleDigitsMap'
-- 
-- 'teens' :: 'Prelude.NumberRule' m 'Prelude.Integer'
-- 'teens' = 'Prelude.ruleFromMap' 'teensMap'
-- 
-- 'doubleDigits' :: 'NumberRule' m 'Prelude.Integer'
-- 'doubleDigits' = 'ruleFromMap' 'Prelude.doubleDigitsMap'
-- 
-- 'zillions' :: 'NumberRule' m 'Prelude.Integer'
-- 'zillions' = 'ruleFromMap' 'zillionsMap'
-- 
-- 'subHundred' :: 'NumberRule' m 'Prelude.Integer'
-- 'subHundred' = 'Control.Monad.msum' $
--   [ (+) 'Control.Applicative.<$>' 'doubleDigits' 'Control.Applicative.<*>' ('Prelude.maybe' 0 'Prelude.id' 'Control.Applicative.<$>' 'Control.Applicative.optional' 'singleDigits'),
--     'teens', 'singleDigits'
--   ]
-- 
-- '_hundred' :: 'NumberRule' m 'Prelude.Integer'
-- '_hundred' = 'fuzzyText' "hundred" 'Control.Monad.>>' 'Control.Monad.return' 100
-- 
-- '_and' :: 'NumberRule' m ()
-- '_and' = 'Control.Monad.msum' $ 'Data.Functor.fmap' 'Dao.Examples.FuzzyStrings.fuzzyText' ('Prelude.words' "and und an nd n") 'Prelude.++' ['Control.Monad.return' ()]
-- 
-- 'hundreds' :: 'NumberRule' m 'Prelude.Integer'
-- 'hundreds' = 'Dao.Rule.bestMatch' $ 'Control.Monad.msum' $
--   [ (\\a hundred b -> a*hundred + b) 'Control.Applicative.<$>' 'singleDigits' 'Control.Applicative.<*>' ('_hundred' 'Control.Applicative.<*' '_and') <*> 'subHundred',
--     (*) 'Control.Applicative.<$>' 'subHundred' 'Control.Applicative.<*>' '_hundred',
--     'subHundred', '_hundred',
--     'numberDigits' 'Control.Monad.>>=' \\i -> 'Control.Monad.guard' (i\<1000) 'Control.Monad.>>' 'Control.Monad.return' i
--   ]
-- 
-- 'numberSentence' :: 'NumberRule' m 'Prelude.Integer'
-- 'numberSentence' = 'Dao.Rule.bestMatch' $ loop 'Data.Monoid.mempty' 0 where
--   loop saidAlready n = do
--     h <- 'hundreds'
--     'Control.Monad.msum' $
--       [( do z <- 'zillions' 'Control.Applicative.<*' '_and'
--             case M.'Data.Map.lookup' z saidAlready of
--               'Prelude.Nothing' -> loop (M.'Data.Map.insert' z () saidAlready) (h\*z + n)
--               'Prelude.Just' () -> 'Dao.Object.throwObject' $ 'Prelude.concat' $
--                 [ "You said ", 'Prelude.maybe' "something-zillion" 'Prelude.show' $ M.'Data.Map.lookup' z 'zillionsMap',
--                   " more than once."
--                 ]
--         ),
--         'Control.Monad.return' (h+n)
--       ]
-- 
-- ----------------------------------------------------------------------------------------------------
--
-- 'digitsToWords' :: 'Prelude.Integer' -> [Strict.'Data.Text.Text']
-- 'digitsToWords' i = toText 'Control.Applicative.<$>' if i==0 then ["zero"] else isNeg $ convert 1 $ breakup $ 'Prelude.abs' i where
--   isNeg        = if i\<0 then ("negative" :) else 'Prelude.id'
--   breakup   i  = if i==0 then [] else 'Prelude.uncurry' (\\a b -> b : breakup a) $ 'Prelude.divMod' i 1000
--   convert z ix = case ix of
--     []   -> []
--     0:ix -> convert (1000 * (z::'Prelude.Integer')) ix
--     i:ix -> do
--       let (hundreds, dd) = 'Prelude.divMod' i 100
--       let (tens,   ones) = 'Prelude.divMod' dd 10
--       let lookup i m = if i==0 then 'Prelude.Just' [] else take 1 . 'Prelude.words' 'Control.Applicative.<$>' M.'Data.Map.lookup' i m
--       'Control.Monad.mplus' (convert (1000\*z) ix) $ 'Prelude.maybe' ["MY BRAIN HURTS!!!"] 'Prelude.id' $
--         if i==100 then 'Prelude.Just' $ 'Prelude.words' "one hundred" else do
--           hundreds <- (\\o -> o 'Control.Monad.>>=' (: ('Prelude.words' "hundred and"))) 'Control.Applicative.<$>' lookup hundreds 'singleDigitsMap'
--           tens <- 'Control.Monad.mplus' (lookup dd 'teensMap') $
--             ('Prelude.++') 'Control.Applicative.<$>' lookup (10*tens) 'doubleDigitsMap' 'Control.Applicative.<*>' lookup ones 'singleDigitsMap'
--           illion <- if z==1 then 'Prelude.Just' [] else M.'Data.Map.lookup' z 'zillionsMap'
--           'Prelude.Just' $ hundreds ++ tens ++ [illion]
-- 
-- 'number' :: 'NumberRule' m 'Object'
-- 'number' = 'Dao.Rule.bestMatch' $
--   (obj . Strict.'Data.Text.unwords' . 'digitsToWords' <$> 'numberDigits') 'Control.Applicative.<|>' ('Dao.Rule.obj' 'Control.Applicative.<$>' 'numberSentence')
-- 
-- main :: IO ()
-- main = do
--   'System.Console.Readline.initialize' -- initialize Readline
--   'Data.Function.fix' $ \\loop -> do
--     q <- 'System.Console.Readline.readline' "number> "
--     case q of
--       'Prelude.Nothing' -> 'Control.Monad.return' ()
--       'Prelude.Just'  q -> do
--         addHistory q
--         q <- pure $ 'Data.Functor.fmap' obj <$> tokenize q
--         if 'Prelude.null' q then 'Control.Monad.return' () else do
--           'Control.Monad.forM_' ('Prelude.zip' q $ 'Data.Functor.fmap' ('Control.Monad.Identity.runIdentity' . 'Dao.Rule.queryAll' number) q)
--             (\\ (q, result) -> case 'Data.Either.partitionEithers' result of
--               ([]   , i:_) -> 'System.IO.print' i
--               ([]   , [] ) -> 'System.IO.putStrLn' $ "Does not appear to be a number: " 'Prelude.++' 'Prelude.show' ('Prelude.head' q)
--               (err:_, [] ) -> 'System.IO.putStrLn' $ "Got an error: " 'Prelude.++' 'Prelude.show' err
--               (err:_, i:_) -> 'System.IO.putStrLn' $ "Got an error: " 'Prelude.++' 'Prelude.show' err 'Prelude.++' "\nDid you mean " 'Prelude.++' 'show' i 'Prelude.++' "?"
--             )
--           loop
-- @
main :: IO ()
main = do
  initialize -- initialize Readline
  fix $ \loop -> do
    q <- readline "number> "
    case q of
      Nothing -> return ()
      Just  q -> do
        addHistory q
        q <- pure $ fmap obj <$> tokenize q
        if null q then return () else do
          forM_ (zip q $ fmap (runIdentity . queryAll number) q)
            (\ (q, result) -> case partitionEithers result of
              ([]   , i:_) -> print i
              ([]   , [] ) -> putStrLn $ "Does not appear to be a number: "++show (head q)
              (err:_, [] ) -> putStrLn $ "Got an error: "++show err
              (err:_, i:_) -> putStrLn $ "Got an error: "++show err++"\nDid you mean "++show i++"?"
            )
          loop

