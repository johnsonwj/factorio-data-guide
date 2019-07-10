module Game.FactorioData.Types where

import Data.Functor.Foldable
import Data.Text

data FactoryF f = Extractor Drill Resource
                -- ^ an item source (drills, pumpjacks, ...). Generally consumes nothing except electricity;
                -- the notable exception is uranium mining which consumes sulfuric acid.

                | Consumer Endpoint
                -- ^ an item sink (labs, rocket silo, weapons)

                | Crafting Crafter Recipe
                -- ^ something that consumes items to create different ones (assemblers, chemical plants, ...)

                | Parallel f [f]
                -- ^ multiple subfactories (e.g. parallel belts/assemblers) whose inputs/outputs are not connected
                -- to each other (but possibly shared, e.g. by one belt)

                | Join f f
                -- ^ one subfactory feeds into another, making its outputs available as inputs to the next

instance Functor FactoryF where
  fmap _ (Extractor d r)  = Extractor d r
  fmap _ (Consumer c)     = Consumer c
  fmap _ (Crafting c r)   = Crafting c r
  fmap t (Parallel f fs)  = Parallel (t f) (t <$> fs)
  fmap t (Join f1 f2)     = Join (t f1) (t f2)

type Factory = Fix FactoryF

-- | One "tick" is the base unit of game time. There are 60 ticks in a game second. One game second should be
-- equal to one second in the real world, unless the game speed is artificially slowed down by mods or by CPU
-- performance issues in very large factories.
data RateUnit = PerTick | PerGameSecond | PerGameMinute deriving (Eq, Ord)
newtype Rate = Rate (Rational, RateUnit)

instance Semigroup Rate where
  Rate (x1, u1) <> r2 = Rate (x1 + x2, u1)
    where
      Rate (x2, _) = convertRate r2 u1

instance Monoid Rate where
  mempty = Rate (0, PerTick)

convertRate :: Rate -> RateUnit -> Rate
convertRate r u2 = Rate (x * conversionFactor u1 u2, u2)
  where
    Rate (x, u1) = r
    conversionFactor u v
      | u == v  = 1
      | u > v   = 1 / conversionFactor v u
    conversionFactor PerTick u = 60 * conversionFactor PerGameSecond u
    conversionFactor PerGameSecond u = 60 * conversionFactor PerGameMinute u

data Drill = Drill { drillName :: Text, miningSpeed :: Rational }

data Resource = Resource { resourceName :: Text, miningTime :: MiningTime, miningFluid :: Maybe MiningFluid }
data MiningTime = MiningTime Rational | MiningYield Int
type MiningFluid = (Text, Int)

data Endpoint = RocketSilo | Lab | Stockpile Rate

data Crafter = Crafter { crafterName :: Text, craftingSpeed :: Rational, craftingCategories :: [Text] }

type ItemCount = (Text, Int)

data Recipe = Recipe { recipeName :: Text, effort :: Rational, ingredients :: [ItemCount], results :: [ItemCount] }

data FactorioData = FactorioData {}
