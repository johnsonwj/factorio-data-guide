module Game.FactorioData.Calc.Types where

import Data.Default
import Data.HashMap.Lazy (HashMap)

import Game.FactorioData.Types

data ProductionRateAnalysisRequest = ProductionRateAnalysisRequest
  { prItems   :: HashMap String Double
  , prOptions :: ProductionOptions
  }

data ProductionOptions = ProductionOptions
  { poAssemblerConfiguration      :: AssemblerConfiguration
  , poOilProcessingConfiguration  :: OilProcessingConfiguration
  , poPowerPlantConfiguration     :: PowerPlantConfiguration

    -- | Module assignment rules; each one is applied until no more compatible
    -- module slots are available before moving on to the next one.
  , poModuleConfiguration         :: [ModuleConfiguration]
  }

instance Default ProductionOptions where
  def = ProductionOptions def def def def

data AssemblerConfiguration = AssemblerConfiguration
  { -- | Maximum allowed assembler level. Report will fail if any precursor
    -- recipes require more advanced assemblers.
    acMaxLevel :: AssemblerLevel

    -- | Minimum allowed assembler level. If not set, recipes will be crafted
    -- in the highest-level assembler (but not higher than 'acMaxLevel').
  , acMinLevel :: Maybe AssemblerLevel
  }

instance Default AssemblerConfiguration where
  def = AssemblerConfiguration AssemblerThree def

data OilProcessingConfiguration = OilProcessingConfiguration
  { -- | Whether or not the Advanced Oil Processing recipe should be used.
    opcAdvancedProcessingEnabled  :: Bool

    -- | Strategy to use for balancing crude / coal liquefaction
    opcRawMaterialStrategy        :: OilProcessingRawMaterialStrategy

    -- | Strategy to use for cracking surplus heavy oil fractions
    opcCrackingStrategy           :: OilProcessingCrackingStrategy
  }

-- | How to balance crude refining vs. coal liquefaction. Default is to
-- use only crude oil, and as much as is needed.
data OilProcessingRawMaterialStrategy = OilProcessingRawMaterialStrategy
  { -- | Whether to prioritize use of crude oil or coal liquefaction
    opcRawPriority    :: OilProcessingRawPriority

    -- | Maximum crude oil yield to pump before starting to liquefy coal
    opcMaxCrudeYield  :: Maybe Int

    -- | Maximum coal per second to liquefy before starting to refine crude
    opcMaxCoalRate    :: Maybe Double
  }

data OilProcessingRawPriority
  = CrudeFirst  -- ^ Use crude oil up to opcMaxCrudeYield, then use coal
  | CoalFirst   -- ^ Use coal liquefaction up to opcMaxCoalRate, then crude

-- | How to crack excess heavy oil fractions, if present. Default is to
-- perform no cracking.
data OilProcessingCrackingStrategy
  = NoCracking

  -- | Excess heavy fractions will be cracked so that surplus fractions are
  -- generated at the same rate, without rebalancing raw material consumption.
  | BalancedSurplus

  -- | Excess heavy fractions will be cracked until they fall below the
  -- specified ceilings. If a ceiling is set for petroleum, raw material
  -- consumption will be rebalanced if cracking the heavier fractions would
  -- push it above that ceiling.
  | SurplusCeilings (Maybe Double) (Maybe Double) (Maybe Double)

  -- | Excess heavy fractions will be cracked, and raw material consumption
  -- rebalanced, to minimize surplus of all oil fractions.
  | MinimizeSurplus

instance Default OilProcessingConfiguration where
  def = OilProcessingConfiguration True def def

instance Default OilProcessingRawMaterialStrategy where
  def = OilProcessingRawMaterialStrategy def def def

instance Default OilProcessingRawPriority where
  def = CrudeFirst

instance Default OilProcessingCrackingStrategy where
  def = NoCracking

data ModuleConfiguration = ModuleConfiguration
  { -- | Which module to use
    mcModuleName :: String

    -- | Maximum number of modules to apply, in each matching machine.
    -- If unset, all remaining compatible slots will be filled.
  , mcModuleCountPerMachine :: Maybe Int

    -- | Maximum number of modules to apply for this rule across the entire
    -- factory. If unset, all compatible slots will be filled before
    -- moving on to the next rule.
  , mcTotalModuleCount      :: Maybe Int

    -- | How to select the machines which will get these modules. Defaults
    -- to 'HighestProduction' (for productivity modules); 'HighestPower'
    -- (for efficiency modules); or 'HighestEffort' (for speed modules).
  , mcModuleTarget :: ModuleTarget
  }

data ModuleTarget
  = HighestProduction
  | HighestPower
  | HighestEffort
  | SpecificRecipe String
    -- ^ This module will go to machines running the specified recipe.
