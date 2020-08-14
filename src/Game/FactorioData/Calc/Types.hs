module Game.FactorioData.Calc.Types where

import Data.Default
import Data.HashMap.Strict
import Data.Text hiding (empty)
import Polysemy.Reader

import Game.FactorioData.Types

-- maybe also a Writer to debug a chained calculation?
-- (in the vein of GHC's "in the second argument of... in the expression... in an equation for...")
type FactorioCalc m a = Reader FactoryConfig m (Either FactorioCalcError a)
type FactorioCalcError = Text

data FactoryConfig = FactoryConfig  { factorioData :: FactorioData
                                    , technologyLevel :: TechnologyLevel
                                    , oilProcessingConfig :: OilProcessingConfiguration
                                    }

data TechnologyEpoch  = Beginning
                      -- ^ All techs that can be unlocked by red science only
                      | Early
                      -- ^ RG techs only
                      | Middle
                      -- ^ RGCB techs only
                      | Advanced
                      -- ^ RGCBYP techs only
                      | Late
                      -- ^ All non-infinite techs

epochSciencePacks :: TechnologyEpoch -> [Text]
epochSciencePacks Beginning = ["automation-science-pack"]
epochSciencePacks Early = "logistic-science-pack" : epochSciencePacks Beginning
epochSciencePacks Middle = "chemical-science-pack" : "military-science-pack" : epochSciencePacks Early
epochSciencePacks Advanced = "utility-science-pack" : "production-science-pack" : epochSciencePacks Middle
epochSciencePacks Late = "space-science-pack" : epochSciencePacks Advanced

-- | All techs up to and including the given epoch, plus more
data TechnologyLevel  = TechnologyLevel TechnologyEpoch [Technology]

data Technology = BasicTech Text | InfiniteTech Text Int

instance Default TechnologyLevel where def = TechnologyLevel Advanced []

-- | What level of oil refining recipes are available?
-- Coal liquefaction must be configured like e.g. "refine up to 1000 crude oil per minute,
-- then use coal liquefaction for any leftover"
data OilProcessingConfiguration = BasicOil | AdvancedOil | CoalLiquefaction LiquefactionScheme Rate

-- | Whether to base calculated coal liquefaction setup on a limited crude consumption vs. limited coal consumption
data LiquefactionScheme = LimitedCoal | LimitedCrude

instance Default OilProcessingConfiguration where def = AdvancedOil

-- | Which things are being produced/consumed, and at what rate
newtype Ledger = Ledger (HashMap Text Rate)

instance Semigroup Ledger where
  Ledger l1 <> Ledger l2 = Ledger $ unionWith (<>) l1 l2

instance Monoid Ledger where
  mempty = Ledger empty
