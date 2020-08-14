module Game.FactorioData.Calc.Manifest where

import Data.Functor.Foldable

import Game.FactorioData.Types
import Game.FactorioData.Calc.Types

data FactoryManifest = FactoryManifest  { deficits  :: Ledger
                                        , surplus   :: Ledger
                                        , consumed  :: Ledger
                                        }

type ManifestCalc = FactorioCalc FactoryManifest

instance Semigroup FactoryManifest where
  m1 <> m2 = FactoryManifest totalDeficits totalSurplus totalConsumed
    where
      totalDeficits = (deficits m1) <> (deficits m2)
      totalSurplus  =  (surplus m1) <> (surplus m2)
      totalConsumed = (consumed m1) <> (consumed m2)

instance Monoid FactoryManifest where
  mempty = FactoryManifest mempty mempty mempty

getManifest :: FactoryConfig -> Factory -> Either FactorioCalcError FactoryManifest
getManifest config factory = runReader (cata foldFactoryManifest factory) config

foldFactoryManifest :: FactoryF ManifestCalc -> ManifestCalc
foldFactoryManifest = undefined
