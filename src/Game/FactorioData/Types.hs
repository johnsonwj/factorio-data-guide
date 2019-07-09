module Game.FactorioData.Types where

import Data.Functor.Foldable
import Data.Text

data FactoryF f = Extractor Drill Resource
                -- ^ an item source (drills, pumpjacks, ...)

                | Consumer ItemConsumer
                -- ^ an item sink (labs, rocket silo, weapons)

                | Crafting Crafter Recipe
                -- ^ something that consumes items to create different ones (assemblers, chemical plants, ...)

                | Transport Logistic
                -- ^ a process that moves items from one place to another, so that the inputs == outputs

                | Parallel f [f]
                -- ^ multiple subfactories (e.g. parallel belts/assemblers) whose inputs/outputs are not connected
                -- to each other (but possibly shared, e.g. by one belt)

                | Join f f
                -- ^ one subfactory feeds into another, making its outputs available as inputs to the next

-- | normal resources like iron ore vs. one with yield mechanics like crude oil
data Resource = Basic ResourceName | Yield ResourceName Int

-- | Type synonyms for entity names
-- todo: expand some of these to support modules
type Drill = Text
type ResourceName = Text
type ItemConsumer = Text
type Crafter = Text
type Recipe = Text

data BeltLevel = Normal | Fast | Express
data Logistic = Belt BeltLevel | LogisticBot | Train

instance Functor FactoryF where
  fmap t (Parallel f fs)  = Parallel (t f) (t <$> fs)
  fmap t (Join f1 f2)     = Join (t f1) (t f2)
  fmap _ (Extractor d r)  = Extractor d r
  fmap _ (Consumer c)     = Consumer c
  fmap _ (Crafting c r)   = Crafting c r
  fmap _ (Transport l)    = Transport l

type Factory = Fix FactoryF
