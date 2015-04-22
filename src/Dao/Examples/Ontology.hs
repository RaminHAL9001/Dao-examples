-- "Dao/Examples/Ontology.hs"  chat with the artificial intelligence.
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

-- | This program is actually an interpreted domain-specific language for modeling ideas and
-- knowledge, and is as an underlying semantics from natural language grammars. Mapping grammars to
-- semantics is not performed in this module as it can be done by a variety of methods -- grammars
-- can programmed by hand, by supervised learning methods, or by unsupervised learning methods such
-- as Darwinian evolution.
module Dao.Examples.Ontology where

import           Prelude hiding ((.), id, lookup)

import           Dao
import qualified Dao.Tree as T

import           Control.Applicative
import           Control.Arrow
import           Control.Category
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State

import           Data.Ix
import           Data.List hiding (lookup)
import           Data.Maybe
import           Data.Monoid
import           Data.Typeable

-- | A monadic type based on 'Dao.Rule.StatefulRule' which evaluates to a pure function.
type Think = StatefulRule Ontology Maybe

----------------------------------------------------------------------------------------------------

-- | Wrap an ordinary data type in a container with a 'Dao.Certainty.Certainty' value to make it a
-- fuzzy data value.
newtype FuzzyData a = FuzzyData (Certainty, a) deriving (Show, Typeable)

instance Eq a => Eq (FuzzyData a) where { (FuzzyData (_, a)) == (FuzzyData (_, b)) = a==b; }

instance Ord a => Ord (FuzzyData a) where
  compare (FuzzyData (xA, a)) (FuzzyData (xB, b)) = compare xB xA <> compare a b

instance Monoid a => Monoid (FuzzyData a) where
  mempty = FuzzyData mempty
  mappend (FuzzyData a) (FuzzyData b) = FuzzyData $ a<>b

instance TestNull a => TestNull (FuzzyData a) where
  nullValue = FuzzyData nullValue
  testNull (FuzzyData a) = testNull a

instance MatchByDistance (FuzzyData NamedRelation) where
  matchDistance a b = if a~>fuzzyData /= b~>fuzzyData then 0.0 else
    MatchDistance $ abs $ a~>fuzzyness - b~>fuzzyness

fuzzyDataTuple :: Monad m => Lens m (FuzzyData a) (Certainty, a)
fuzzyDataTuple = newLens (\ (FuzzyData a) -> a) (\a _ -> FuzzyData a)

-- | A lens on the 'Dao.Certainty.Certainty' value of a 'FuzzyData' type.
fuzzyness :: Monad m => Lens m (FuzzyData a) Certainty
fuzzyness = fuzzyDataTuple >>> tuple0

-- | A lens on the data value of a 'FuzzyData' type.
fuzzyData :: Monad m => Lens m (FuzzyData a) a
fuzzyData = fuzzyDataTuple >>> tuple1

----------------------------------------------------------------------------------------------------

-- | 'NamedRelations' can form circular references. When the 'reasoning' lens is used to dereference
-- a 'NamedRelation', rather than fail right away, this data type is returned, requiring the calling
-- context to decide if a 'CircularReference' should result in an exception being thrown.
data SubstitutionPath
  = SubstitutionPath (Array NamedRelation) Idea
  | CircularReference (Array NamedRelation)
  deriving (Eq, Ord, Show, Typeable)

circularReferenceError :: (Monad m, MonadError ErrorObject m) => Array NamedRelation -> m e
circularReferenceError o = throwError [obj "circular reference", obj $ fmap obj o]

ideaAtPath :: (Monad m, MonadError ErrorObject m) => Lens m SubstitutionPath Idea
ideaAtPath =
  newLensM
    (\     o -> case o of
        SubstitutionPath _ o -> return o
        CircularReference  o -> circularReferenceError o
    )
    (\idea o -> case o of
        SubstitutionPath a _ -> return $ SubstitutionPath a idea
        CircularReference  o -> circularReferenceError o
    )

substitutionPath :: Monad m => Lens m SubstitutionPath (Array NamedRelation)
substitutionPath =
  newLens
    (\    o -> case o of
        SubstitutionPath  arr _ -> arr
        CircularReference arr   -> arr
    )
    (\arr o -> case o of
        SubstitutionPath  _ o -> SubstitutionPath  arr o
        CircularReference _   -> CircularReference arr
    )

----------------------------------------------------------------------------------------------------

-- | 'Idea's are a form of hard data, but you can have many different names refering to an 'Idea',
-- which can create logical relationships between pieces of data.
data NamedRelation = Myself | Relation (Array Object)
  deriving (Eq, Ord, Show, Typeable)

instance SimpleData NamedRelation where
  simple     o = case o of { Myself -> simple (); Relation o -> simple o; }
  fromSimple o = mplus (fromSimple o >>= \ () -> return Myself) (Relation <$> fromSimple o)

instance ObjectData NamedRelation where
  obj     = obj . simple
  fromObj = fromObj >=> fromSimple

instance TestNull NamedRelation where
  nullValue = Myself
  testNull Myself = True
  testNull _ = False

instance MatchByDistance NamedRelation where
  matchDistance a b = if a==b then exactSame else mempty

-- | This lens is used to 'Dao.Lens.fetch' or 'Dao.Lens.alter' an 'Idea' by it's 'NamedRelation'.
-- However 'NamedRelation's can refer to any 'Idea', even other 'NamedRelation's. When a
-- 'NamedRelation' refers to another 'NamedRelation', this function will dereference the reference
-- to the reference, and will keep on doing that until it finds an infinite loop of references, or
-- until it finds an 'Idea' that is not a 'NamedRelation'.
reasoning :: Lens Think NamedRelation SubstitutionPath
reasoning = Lens $ \f -> StateT $ \name -> flip (,) name <$> loop f [] name where
  loop :: (Maybe (SubstitutionPath -> Think SubstitutionPath)) -> [NamedRelation] -> NamedRelation -> Think SubstitutionPath
  loop f refs a = case a of
    Myself     -> gets $ SubstitutionPath (array $ Myself : refs) . PersonIdea . (~> myself)
    Relation a -> do
      refs <- pure $ Relation a : refs
      let lens   = myself >>> personsKnowledge >>> T.path (elems a)
      let next b = if elem b refs then return $ CircularReference $ array refs else loop f refs b
      b <- gets (~> lens)
      case b of
        Nothing -> mzero
        Just  b -> case b of
          LabeledIdeas g -> returnGroup g >>= next
          b              -> SubstitutionPath (array refs) <$> case f of
            Nothing -> return b
            Just  f -> f (SubstitutionPath (array refs) b) >>= \b -> case b of
              CircularReference refs -> circularReferenceError refs
              SubstitutionPath  _  b -> modify (by [lens <~ Just b]) >> return b

-- | This lens is used 'Dao.Lens.fetch' or 'Dao.Lens.alter' an 'Idea' referred to by a
-- 'NamedRelation', and unlike the 'reasoning' lens, this lens will 'Dao.Lens.fetch' or
-- 'Dao.Lens.alter' the 'Idea' even if it is itself a 'NamedRelation'.
assumption :: Lens Think NamedRelation Idea
assumption = Lens $ \f -> StateT $ \name -> fmap (flip (,) name) $ case f of
  Nothing -> case name of
    Myself -> gets $ PersonIdea . (~> myself)
    Relation name -> 
      gets (~> (myself >>> personsKnowledge >>> T.path (elems name))) >>= maybe mzero return
  Just  f -> case name of
    Myself -> do
      e <- gets (~> myself) >>= f . PersonIdea
      e <- case e of { PersonIdea e -> return e; _ -> mzero; }
      modify $ by [myself <~ e]
      return (PersonIdea e)
    Relation name -> let lens = myself >>> personsKnowledge in do
      e <- gets (~> (lens >>> T.path (elems name))) >>= f . fromMaybe Blank
      modify $ by [lens $= T.insert (elems name) e]
      return e

----------------------------------------------------------------------------------------------------

-- | 'NamedRelations' are 'Idea's that can refer to other 'Idea's. However these references
-- themselves are fuzzy. This data type stores a set of unique 'NamedRelations', each with a
-- 'Certainty' value indicating how certain the relationship is.
newtype RelationGroup = RelationGroup (Maybe Certainty, T.Tree Object Certainty)
  deriving (Eq, Ord, Show, Typeable)

instance TestNull RelationGroup where
  nullValue = RelationGroup (Nothing, nullValue)
  testNull (RelationGroup (myself, set)) = isNothing myself && testNull set

instance Monoid RelationGroup where
  mempty = nullValue
  mappend (RelationGroup (a1, b1)) (RelationGroup (a2, b2)) = RelationGroup $
    (mappend <$> a1 <*> a2 <|> a1 <|> a2, T.unionWith mappend b1 b2)

instance MatchByDistance RelationGroup where
  matchDistance (RelationGroup (_, a)) (RelationGroup (_, b)) = matchDistance a b

relationGroupTuple :: Monad m => Lens m RelationGroup (Maybe Certainty, T.Tree Object Certainty)
relationGroupTuple = newLens (\ (RelationGroup o) -> o) (\o _ -> RelationGroup o)

relationIncludesMyself :: Monad m => Lens m RelationGroup (Maybe Certainty)
relationIncludesMyself = relationGroupTuple >>> tuple0

relationSet :: Monad m => Lens m RelationGroup (T.Tree Object Certainty)
relationSet = relationGroupTuple >>> tuple1

includeRelations :: [(NamedRelation, Certainty)] -> RelationGroup -> RelationGroup
includeRelations list = mappend $ mconcat $ list >>= \o -> case o of
  (Myself      , o) -> [RelationGroup (Just  o, nullValue)]
  (Relation rel, o) -> [RelationGroup (Nothing, T.singleton (elems rel) o)]

-- | Get the list of people from the 'RelationGroup' ordered by how closely the query that produced
-- this group matched.
relationsFromGroup :: RelationGroup -> [(NamedRelation, Certainty)]
relationsFromGroup (RelationGroup (myself, group)) = (++ ((,) Myself <$> maybeToList myself)) $
  sortBy (\ (a1, b1) (a2, b2) -> compare (abs b1) (abs b2) <> compare a1 a2) $
    ((first $ Relation . array) <$> (T.assocs T.DepthFirst group)) ++ maybe [] (return . (,) Myself) myself

-- | Use 'relationsFromGroup' and 'Dao.Logic.superState' to return every 'Person' in the group in
-- "parallel" computations.
returnGroup :: RelationGroup -> Think NamedRelation
returnGroup g = superState $ \st -> (flip (,) st) <$> (fst <$> relationsFromGroup g)

----------------------------------------------------------------------------------------------------

-- | The ontology for the AI is represented by the 'Idea' data type.
data Idea
  = Blank
  | PersonIdea    Person
  | StoryIdea     Story
  | LabeledIdeas  RelationGroup
  | SomeData      Object
  deriving (Eq, Ord, Show, Typeable)

instance MatchByDistance Idea where
  matchDistance a b =
    let storyDist    = fromRational . toRational . length . storyToList
        personDist   = fromRational . toRational . T.size . (~> personsKnowledge)
        relationDist = fromRational . toRational . T.size . (~> relationSet)
        dataDist     = const 1.0
        blankDist    = 1.0
    in  case a of
          Blank           -> case b of
            Blank           -> 0.0 :: MatchDistance
            PersonIdea    b -> blankDist + personDist   b :: MatchDistance
            StoryIdea     b -> blankDist + storyDist    b :: MatchDistance
            LabeledIdeas  b -> blankDist + relationDist b :: MatchDistance
            SomeData      b -> blankDist + dataDist b :: MatchDistance
          PersonIdea    a -> case b of
            Blank           -> personDist a + blankDist
            PersonIdea    b -> matchDistance a b
            StoryIdea     b -> personDist a + storyDist    b
            LabeledIdeas  b -> personDist a + relationDist b
            SomeData      b -> personDist a + dataDist     b
          StoryIdea     a -> case b of
            Blank           -> storyDist a + blankDist
            PersonIdea    b -> storyDist a + personDist   b
            StoryIdea     b -> matchDistance a b
            LabeledIdeas  b -> storyDist a + relationDist b
            SomeData      b -> storyDist a + dataDist     b
          LabeledIdeas  a -> case b of
            Blank           -> relationDist a + blankDist
            PersonIdea    b -> relationDist a + personDist b
            StoryIdea     b -> relationDist a + storyDist  b
            LabeledIdeas  b -> matchDistance a b
            SomeData      b -> relationDist a + dataDist   b
          SomeData      a -> case b of
            Blank           -> dataDist a + blankDist
            PersonIdea    b -> dataDist a + personDist b
            StoryIdea     b -> dataDist a + storyDist  b
            LabeledIdeas  b -> dataDist a + relationDist b
            SomeData      b -> if a==b then 0.0 else 1.0

-- | Lenses for accessing specific sub-types of the 'Idea' data type are called 'TypeOfIdea' types.
type TypeOfIdea o = forall m . (Monad m, MonadPlus m, MonadError ErrorObject m) => Lens m Idea o

-- | Extracts the 'Person' parameter from a 'SubstitutionPath' data type, or throws an exception if it
-- is a 'CircularReference'
personType :: TypeOfIdea Person
personType =
  newLensM
    (\  idea -> case idea of { PersonIdea o -> return o; _ -> mzero; })
    (\o idea -> case idea of { PersonIdea _ -> return $ PersonIdea o; _ -> mzero; })

-- | Extracts the 'Person' parameter from a 'SubstitutionPath' data type, or throws an exception if it
-- is a 'CircularReference'
storyType :: TypeOfIdea Story
storyType =
  newLensM
    (\  idea -> case idea of { StoryIdea o -> return o; _ -> mzero; })
    (\o idea -> case idea of { StoryIdea _ -> return $ StoryIdea o; _ -> mzero; })

-- | Extracts the 'Object' parameter from a 'SubstitutionPath' data type, or throws an exception if it
-- is a 'CircularReference'
objectType :: TypeOfIdea Object
objectType =
  newLensM
    (\  idea -> case idea of { SomeData o -> return o; _ -> mzero; })
    (\o idea -> case idea of { SomeData _ -> return $ SomeData o; _ -> mzero; })

-- | Extracts the 'Object' parameter from a 'SubstitutionPath' data type, or throws an exception if it
-- is a 'CircularReference'
relationGroupType :: TypeOfIdea RelationGroup
relationGroupType =
  newLensM
    (\  idea -> case idea of { LabeledIdeas o -> return o; _ -> mzero; })
    (\o idea -> case idea of { LabeledIdeas _ -> return $ LabeledIdeas o; _ -> mzero; })

-- | The 'with' function is used to modify state data in the AI's knowledge base. Provide a
-- 'NamedRelation' refering to the data to be modified, provide a 'TypeOfIdea' (or
-- 'Control.Category.id' if you want to operate on the 'Idea' data directly) to indicate the type of
-- 'Idea' that is expected, and provide a function for operating on the 'Idea'. If the 'Idea'
-- referred to by the 'NamedRelation' is not the expected 'TypeOfIdea', this function evaluates to
-- 'Control.Monad.mzero'.
withName :: NamedRelation -> TypeOfIdea idea -> (idea -> Think (o, idea)) -> Think o
withName name typ f = do
  what <- fetch reasoning name
  idea <- fetch (ideaAtPath >>> typ) what
  name <- fromMaybe Myself . (! 0) <$> fetch substitutionPath what -- get the nearest 'NamedRelation'
  (o, idea) <- f idea
  what <- update (ideaAtPath >>> typ) idea what
  update reasoning what name
  return o

----------------------------------------------------------------------------------------------------

class Executable o where { execute :: o -> Think Idea; }

----------------------------------------------------------------------------------------------------

-- | Encodes what is known about objects that exist in the universe, where these objects may be
-- experience 'Change's (state transitions) through the course of their existence. The 'Person' data
-- type is used to model everything, whether it is sentient, mechanical, animal, plant, or even
-- non-sentient, non-living things. This reflects the human tendency to see agency and intent in
-- everything.
newtype Person = Person (FuzzyData (T.Tree Object Idea)) deriving (Eq, Ord, Show, Typeable)

instance TestNull Person where
  nullValue = Person nullValue
  testNull (Person a) = testNull a

instance Monoid Person where
  mempty = nullValue
  mappend (Person (FuzzyData (a1, b1))) (Person (FuzzyData (a2, b2))) =
    let f = fromMaybe Blank in Person $ FuzzyData $
      (a1<>a2, T.mergeWith (\a b -> tryToAppendIdeas (f a) (f b)) id id b1 b2)

instance MatchByDistance Person where
  matchDistance a b = treeDistanceWith matchDistance (a~>personsKnowledge) (b~>personsKnowledge)

personLens :: Monad m => Lens m Person (FuzzyData (T.Tree Object Idea))
personLens = newLens (\ (Person a) -> a) (\a _ -> Person a)

-- | How certain we are that a person is real. A person that isn't real is fictional or
-- hypothetical, but we may still reason about this person as if they were real.
personIsReal :: Monad m => Lens m Person Certainty
personIsReal = personLens >>> fuzzyness

personsKnowledge :: Monad m => Lens m Person (T.Tree Object Idea)
personsKnowledge = personLens >>> fuzzyData

-- | It is possible to combine some ideas, especially when merging two 'Dao.Tree.Tree's of ideas
-- using 'Dao.Tree.mergeWith'. This function is designed for that purpose. If two ideas are similar,
-- they are merged, but if they are not similar, this function evaluates to 'Prelude.Nothing'.
tryToAppendIdeas :: Idea -> Idea -> Maybe Idea
tryToAppendIdeas a b = case (a, b) of
  (Blank          , b              )        -> Just b
  (a              , Blank          )        -> Just a
  (PersonIdea    a, PersonIdea    b)        -> Just $ PersonIdea $ a<>b
  (StoryIdea     a, StoryIdea     b)        -> Just $ StoryIdea $ a<>b
  (LabeledIdeas  a, LabeledIdeas  b)        -> Just $ LabeledIdeas $ a<>b
  (SomeData      a, SomeData      b) | a==b -> Just $ SomeData a
  _                                         -> Nothing

----------------------------------------------------------------------------------------------------

-- | A 'Story' is a sequence of 'Change's. The 'Story' data type allows for moving back or forth in
-- the sequence. The present is a list of 'Change's that can be converted to an event.
data Story = Story (Certainty, [Change], [Change]) deriving (Eq, Ord, Show, Typeable)

instance TestNull Story where { nullValue = Story nullValue; testNull (Story a) = testNull a; }

instance Monoid Story where { mempty = nullValue; mappend (Story a) (Story b) = Story $ a<>b; }
 
instance MatchByDistance Story where
  matchDistance a b =
    case matchPermutationPattern 0.333 matchDistance (storyToList a) (storyToList b) of
      []       -> mempty
      (c, _):_ -> c

instance Executable Story where
  execute s = 
    let loop ox z = case ox of { [] -> return z; o:ox -> execute o >>= loop ox; }
    in  loop (storyToList s) Blank

storyTuple :: Monad m => Lens m Story (Certainty, [Change], [Change])
storyTuple = newLens (\ (Story o) -> o) (\o _ -> Story o)

-- | How certain are we that this story is not fictional?
notFictional :: Monad m => Lens m Story Certainty
notFictional = storyTuple >>> tuple0

past :: Monad m => Lens m Story [Change]
past = storyTuple >>> tuple1

present :: Monad m => Lens m Story (Maybe Change)
present = newLens (\ (Story (_, bx, _)) -> if null bx then Nothing else Just $ head bx) $
  (\b s@(Story (a, bx, c)) -> maybe s (\b -> Story (a, b:bx, c)) b)

future :: Monad m => Lens m Story [Change]
future = storyTuple >>> tuple2

storyToList :: Story -> [Change]
storyToList (Story (_, a, b)) = reverse a ++ b

-- | Stories have a concept of focus, which is kind of like the idea of a cursor used in video
-- playback. This function moves the focus of the story to the start of the 'Story'.
beginningOfStory :: Story -> Story
beginningOfStory (Story (a, b, c)) = Story (a, [], reverse b ++ c)

-- | Stories have a concept of focus, which is kind of like the idea of a cursor used in video
-- playback. This function moves the focus of the story to the end of the story.
endOfStory :: Story -> Story
endOfStory (Story (a, b, c)) = Story (a, b ++ reverse c, [])

-- | Blindly move back or forward through a story an 'Prelude.Integer' number of steps.
storyMove :: Integer -> Story -> Story
storyMove n s@(Story (cer, past, future)) = if n==0 then s else
  if n<0
  then if null past   then s else storyMove (n+1) $ Story (cer, tail past, head past : future)
  else if null future then s else storyMove (n-1) $ Story (cer, past++[head future], tail future)

-- | Search through a 'Story's 'future' for some part of the 'Story' that matches a 'Story'
-- pattern. Returns an updated 'Story' along with a Dao.Certainty.MatchDistance indicating how closely the
-- pattern matched the resulting portion of the 'Story'.
storyForward :: Story -> Story -> Think (MatchDistance, Story)
storyForward pat (Story (fic, past, futr)) = do
  lim <- gets (~> (matchThreshold >>> distanceSimilarity >>> average))
  superState $ \st -> 
    (\ (dist, (fore, aft)) -> ((dist, Story (fic, past++reverse fore, aft)), st)) <$>
      matchPermutationPattern lim matchDistance (storyToList pat) futr

-- | Search through a 'Story's 'past' for some part of the 'Story' that matches a 'Story'
-- pattern. Returns an updated 'Story' along with a Dao.Certainty.MatchDistance indicating how closely the
-- pattern matched the resulting portion of the 'Story'.
storyBackward :: Story -> Story -> Think (MatchDistance, Story)
storyBackward pat (Story (fic, past, futr)) = do
  lim <- gets (~> (matchThreshold >>> distanceSimilarity >>> average))
  superState $ \st -> 
    (\ (dist, (fore, aft)) -> ((dist, Story (fic, aft, reverse fore ++ futr)), st)) <$>
      matchPermutationPattern lim matchDistance (storyToList pat) past

----------------------------------------------------------------------------------------------------

data StoryOpCode
  = Evaluate          -- ^ Evaluate the current story on a 'Person' parameter.
  | Evaluate1         -- ^ Evaluate the current story on a 'Person' parameter.
  | ClearStory        -- ^ clear the contents of the current story.
  | StoryToEnd        -- ^ Think back to the previous part of the story.
  | StoryToBeginning  -- ^ Think back to the very beginning of this story.
  | CopyStoryTo       NamedRelation -- ^ copy the current story to another story
  | CopyFutureTo      NamedRelation -- ^ copy the the future of the current story to another story
  | CopyPastTo        NamedRelation -- ^ copy the past of the current story to another story
  | CopyMatchingStory NamedRelation NamedRelation
    -- ^ using a the first parameter story as a pattern, copy the matching portion of the current
    -- story to the second parameter target story.
  | StoryForward      NamedRelation -- ^ Think forward to a given event in this story.
  | StoryBackward     NamedRelation -- ^ Think back to a given 'Change' in this story.
  deriving (Eq, Ord, Show, Typeable)

newtype StoryOp = StoryOp (NamedRelation, StoryOpCode) deriving (Eq, Ord, Show, Typeable)

instance MatchByDistance StoryOpCode where
  matchDistance a b = if a==b then exactSame else mempty

instance MatchByDistance StoryOp where
  matchDistance a b = if a==b then exactSame else mempty

storyOpLens :: Monad m => Lens m StoryOp (NamedRelation, StoryOpCode)
storyOpLens = newLens (\ (StoryOp o) -> o) (\o _ -> StoryOp o)

instance Executable StoryOp where
  execute (StoryOp (name, o)) = withName name storyType $ \s@(Story (certain, pa, fu)) -> do
    let combine name f = withName name storyType $ \s -> let t = f s in return ((StoryIdea t, t), t)
    let storyShift f pat = (\ (_, s) -> ((StoryIdea s, pat), s)) <$> f pat s
    case o of
      Evaluate         -> do
        let loop ox p = case ox of { [] -> return p; o:ox -> execute o >>= loop ox; }
        flip (,) s <$> loop (reverse pa ++ fu) Blank
      Evaluate1        -> case fu of
        []   -> mzero
        o:ox -> flip (,) (Story (certain, o:pa, ox)) <$> execute o
      ClearStory                 -> return (StoryIdea s, nullValue)
      StoryToEnd                 -> return $ (\s -> (StoryIdea s, s)) $ endOfStory s
      StoryToBeginning           -> return $ (\s -> (StoryIdea s, s)) $ beginningOfStory s
      CopyStoryTo           name -> combine name (<> s)
      CopyFutureTo          name -> combine name $ \t -> Story (certain, pa, t~>future ++ fu)
      CopyPastTo            name -> combine name $ \t -> Story (certain, t~>past ++ pa, fu)
      CopyMatchingStory pat name -> withName pat storyType $ \pat -> do
        lim <- gets (~> (matchThreshold >>> distanceSimilarity >>> average))
        let matched = matchPermutationPattern lim matchDistance (storyToList pat) (reverse pa ++ fu)
        msum $ matched >>= \ (_, (ox, _)) -> if null ox then [] else
          [ withName name storyType $ \ (Story (cer, pa, fu)) ->
              return (((StoryIdea $ Story (cer, [], ox), Story (cer, pa, ox++fu)), pat), s)
          ]
      StoryForward      pat     -> withName pat storyType $ storyShift storyForward 
      StoryBackward     pat     -> withName pat storyType $ storyShift storyBackward

----------------------------------------------------------------------------------------------------

-- | This is a scripting language used to modify a 'Person' data structure. All operations are
-- performed on the 'Person' state contained in the current 'Ontology' monad.
data KnowledgeOpCode
  = SaveState NamedRelation
    -- ^ copy the knowledge of the current 'Person' state into a 'NamedRelation' within the current
    -- state.
  | Assume NamedRelation Idea
    -- ^ initialize knowledge at a given address with the given 'Idea'. If an 'Idea' with the given
    -- 'NamedRelation' already exists, the existing idea will not be replaced, and the
    -- 'NamedRelation' will store both 'Idea's simultaneously. This will complicate retrieval of
    -- ideas when refered to by 'NamedRelation', as doing so will return both 'Idea's resulting in a
    -- parallel computation with both 'Idea's.
  | SelectPerson  (Statement Person) NamedRelation
    -- ^ using a @('Statement' 'Person')@ as a description of the person, search the knowledge base
    -- for 'Person's and create a 'LabeledIdeas' of all matching persons in the given
    -- 'NamedRelation'.
  | ModifyKnowledge SetInfixOp NamedRelation
    -- ^ Perform 'SetAND', 'SetOR', or 'SetDEL' on the current 'Person' state with another 'Person'
    -- operand referred to by the given 'NamedRelation'. 'SetOR' will combine it with the knowledge
    -- known by the currnet 'Person' state, 'SetAND' will delete any knowledge not known by both
    -- this 'Person' operand and the current 'Person' state, 'SetDEL' will the knowledge known by
    -- this 'Person' operand from the current 'Person' state, 
  | UpdatePerson  NamedRelation NamedRelation
    -- ^ Select a 'Person' and modify the current state to assume the identity of the 'Person', then
    -- execute a 'Story' on that 'Person', then return to the original state, keeping the
    -- modifications on the selected 'Person'. In other words, think what would happen if a 'Person'
    -- experienced the events of a given 'Story'.
  deriving (Eq, Ord, Show, Typeable)

data SetInfixOp = SetAND | SetOR | SetDEL deriving (Eq, Ord, Show, Typeable, Bounded, Enum, Ix)

-- | Performs some operation on a person named by the 'NamedRelation'
newtype KnowledgeOp = KnowledgeOp (NamedRelation, KnowledgeOpCode)
  deriving (Eq, Ord, Show, Typeable)

instance TestNull KnowledgeOpCode where
  nullValue = SaveState nullValue
  testNull o = case o of { SaveState Myself -> True; _ -> False; }

instance MatchByDistance KnowledgeOpCode where
  matchDistance a b = if a==b then exactSame else mempty

instance MatchByDistance KnowledgeOp where
  matchDistance a b = if a==b then exactSame else mempty

instance Executable KnowledgeOp where
  execute (KnowledgeOp (who, op)) = withName who personType $ \p -> case op of
    SaveState name -> do
      unless (who==name) $ case name of
        Myself        -> modify $ by [myself <~ p]
        Relation name -> withName (Relation name) personType $ const $ return ((), p)
      return (PersonIdea p, p)
    Assume  name o -> do
      let err = throwError [obj "name already allocated", obj name]
      case name of
        Myself        -> err
        Relation name -> do
          let lens = myself >>> personsKnowledge >>> T.path (elems name)
          alreadyExists <- gets (~> lens)
          case alreadyExists of
            Nothing -> modify $ by [lens <~ Just o]
            Just  _ -> void err
          return (o, p)
    SelectPerson pat name -> do
      lim   <- gets (~> (matchThreshold >>> distanceSimilarity >>> average))
      knows <- gets (~> (myself >>> personsKnowledge))
      (people, assocs) <- pure $ unzip $ do
        (who, person) <- T.assocs T.DepthFirst knows
        person <- case person of { PersonIdea p -> [p]; _ -> []; }
        dist <- pure $ matchStatement matchDistance pat person
        guard $ (dist~>distanceSimilarity~>average) < lim
        [((person, dist), (who, (~> distanceSimilarity) $ invertDistance dist))]
      group <- pure $ LabeledIdeas $ RelationGroup (Nothing, T.fromList assocs)
      update assumption group name
      superState $ \st ->
        zip ((,) group . fst <$> sortBy (\a b -> compare (snd a) (snd b)) people) (repeat st)
    ModifyKnowledge  op name -> do
      let setOp name f = withName name personType $ \q -> return $
            ((PersonIdea q, with p [personsKnowledge $= f (q~>personsKnowledge)]), q)
      case op of
        SetDEL -> setOp name T.difference
        SetAND -> setOp name T.intersection
        SetOR  -> setOp name T.union
    UpdatePerson  story person ->
      withName person personType $ \someoneElse ->
        withName story storyType $ \story -> do
          -- this is the empathy mechanism: suppose you are someone else
          (result, someoneElse) <- hypothesize myself someoneElse $ execute story
          me <- gets (~> myself)
          return (((result, me), someoneElse), story)

----------------------------------------------------------------------------------------------------

data Change = OnKnowledgeOp KnowledgeOp | OnStoryOp StoryOp
  deriving (Eq, Ord, Show, Typeable)

instance Executable Change where
  execute o = case o of { OnKnowledgeOp o -> execute o; OnStoryOp o -> execute o; }

instance Executable (Array Change) where
  execute o =
    let loop ox o = case ox of { [] -> return o; (o:ox) -> execute o >>= loop ox }
    in  loop (elems o) Blank

instance MatchByDistance Change where
  matchDistance a b = case a of
    OnKnowledgeOp a -> case b of
      OnKnowledgeOp b -> matchDistance a b
      OnStoryOp     _ -> mempty
    OnStoryOp   a   -> case b of
      OnKnowledgeOp _ -> mempty
      OnStoryOp     b -> matchDistance a b

----------------------------------------------------------------------------------------------------

newtype LocalVars = LocalVars (T.Tree Object (Array Object)) deriving (Eq, Ord, Show, Typeable)

localVarsTree :: Monad m => Lens m LocalVars (T.Tree Object (Array Object))
localVarsTree = newLens (\ (LocalVars o) -> o) (\o _ -> LocalVars o)

instance TestNull LocalVars where
  nullValue = LocalVars nullValue
  testNull (LocalVars o) = testNull o

instance Monoid LocalVars where
  mempty = nullValue
  mappend (LocalVars a) (LocalVars b) = LocalVars $ T.union b a

----------------------------------------------------------------------------------------------------

-- | A 'Ontology' holds knowledge of two kinds of information: First there is a 'Dao.Tree.Tree'
-- of 'ideas' that models what the AI knows about the universe around it. Second is a knowledge of
-- language which it uses to learn knowledge about the universe around it, and to communicate what
-- it knows.
newtype Ontology = Ontology (MatchDistance, Person)
  deriving (Eq, Ord, Show, Typeable)

instance TestNull Ontology where
  nullValue = Ontology nullValue
  testNull (Ontology o) = testNull o

instance Monoid Ontology where
  mempty = nullValue
  mappend (Ontology a) (Ontology b) = Ontology $ a<>b

ontologyTuple :: Monad m => Lens m Ontology (MatchDistance, Person)
ontologyTuple = newLens (\ (Ontology o) -> o) (\o _ -> Ontology o)

-- | A lens for the 'Double'-percision match threshold parameter currently set for the
-- 'Ontology'.
matchThreshold :: Monad m => Lens m Ontology MatchDistance
matchThreshold = ontologyTuple >>> tuple0

-- | A lens for the 'Person' the 'Ontology'
myself :: Monad m => Lens m Ontology Person
myself = ontologyTuple >>> tuple1

-- | Given an 'myself' or 'Language' state data and a lens to access the data in
-- the current state, use it temporarily as the current state and execute a computation.
hypothesize :: PureLens Ontology p -> p -> Think o -> Think (o, p)
hypothesize lens before f = do
  original <- gets (~> lens)
  result   <- modify (by [lens <~ before]) >> f
  after    <- gets (~> lens)
  modify (by [lens <~ original]) >> return (result, after)

----------------------------------------------------------------------------------------------------

