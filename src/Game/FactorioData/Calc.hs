module Game.FactorioData.Calc where

import Control.Monad.Reader
import Data.List (nub)
import Data.Text

import Game.FactorioData.Types
import Game.FactorioData.Calc.Types

unlockedSciencePacks :: FactorioCalc [Text]
unlockedSciencePacks = do
  TechnologyLevel epoch addlTechs <- asks technologyLevel
  addlUnlockedPacks <- do
    return _ -- filter factorioData for addlTechs; flatMap to unlocked recipes; filter for *-science-pack
  return . Right $ nub (epochSciencePacks epoch ++ addlUnlockedPacks)

labSpeedBonus :: FactorioCalc Rational
labSpeedBonus = undefined
-- based on unlocked techs

fastestSciencePackConsumption :: FactorioCalc Rate
fastestSciencePackConsumption = undefined
-- map unlocked techs -> base pack consumption, take max, times labSpeedBonus

maxLabConsumption :: FactorioCalc Ledger
maxLabConsumption = undefined
