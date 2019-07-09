module Game.FactorioData.Calc.Types where

import Data.Default
import Data.Text

import Game.FactorioData.Types

data FactoryConfig = FactoryConfig  { technologyLevel :: TechnologyLevel
                                    , oilProcessingConfig :: OilProcessingConfiguration
                                    }

instance Default FactoryConfig where def = FactoryConfig def def

data TechnologyEpoch  = Beginning
                      -- ^ All techs that can be unlocked by red science only
                      | Early
                      -- ^ RG techs only
                      | Middle
                      -- ^ RGB techs only
                      | Advanced
                      -- ^ RGBYP techs only
                      | Late
                      -- ^ All non-infinite techs

-- | All techs up to and including the given epoch, plus more
data TechnologyLevel  = TechnologyLevel TechnologyEpoch [Technology]

data Technology = BasicTech Text | InfiniteTech Text Int

instance Default TechnologyLevel where def = TechnologyLevel Advanced []

-- | What level of oil refining recipes are available?
-- Coal liquefaction must be configured like e.g. "refine up to 1000 crude oil per minute,
-- then use coal liquefaction for any leftover"
data OilProcessingConfiguration = BasicOil | AdvancedOil | CoalLiquefaction LiquefactionScheme RateUnit Int

-- | Whether to base calculated coal liquefaction setup on a limited crude consumption vs. limited coal consumption
data LiquefactionScheme = LimitedCoal | LimitedCrude

-- | One "tick" is the base unit of game time. There are 60 ticks in a game second. One game second should be
-- equal to one second in the real world, unless the game speed is artificially slowed down by mods or by CPU
-- performance issues in very large factories.
data RateUnit = PerTick | PerGameSecond | PerGameMinute

instance Default OilProcessingConfiguration where def = AdvancedOil
