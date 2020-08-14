module Game.FactorioData.Foo  ( Production (items), production
                              , ItemQuantity (), itemQuantity
                              ) where

import Data.Default
import Data.Foldable (foldl')
import Data.HashMap.Strict as M (HashMap, unionWith, empty, insertWith, fromList)
import Data.List.NonEmpty as NEL (NonEmpty((:|)), fromList)
import Data.Ratio
import Data.Text (Text)
import Data.Functor.Foldable
import Polysemy
import Polysemy.Reader

data ItemQuantity = ItemQuantity Text Int

itemQuantity :: Text -> Int -> ItemQuantity
itemQuantity itemName count
  | count <= 0  = error "item quantity must be strictly greater than zero"
  | otherwise   = ItemQuantity itemName count

singleItem :: Text -> NonEmpty ItemQuantity
singleItem name = ItemQuantity name 1 :| []

singleItems :: [Text] -> NonEmpty ItemQuantity
singleItems names = NEL.fromList (map (\n -> ItemQuantity n 1) names)

singleItem' :: Text -> Int -> NonEmpty ItemQuantity
singleItem' name q = ItemQuantity name q :| []

itemList :: [(Text, Int)] -> NonEmpty ItemQuantity
itemList qs = NEL.fromList (map (\(n, q) -> itemQuantity n q) qs)

-- don't export ctor; empty po's are not allowed
newtype Production = Production { items :: HashMap Text Int }

instance Semigroup Production where
  po1 <> po2 = Production $ unionWith (+) (items po1) (items po2)

production :: NonEmpty ItemQuantity -> Production
production = foldl' (\(Production p) (ItemQuantity itemName q) -> Production (insertWith (+) itemName q p)) (Production empty)

isResource :: Text -> Bool
isResource "iron-ore" = True
isResource "copper-ore" = True
isResource _ = False

data RecipeType = Standard | Smelting

data Recipe = Recipe  { recipeResults :: Production
                      , recipeIngredients :: Production
                      , recipeTime :: Int -- ticks
                      , recipeType :: RecipeType
                      }

recipe :: RecipeType -> NonEmpty ItemQuantity -> NonEmpty ItemQuantity -> Int -> Recipe
recipe rt ingredients results effort = Recipe (production results) (production ingredients) effort rt

type RecipeBook = HashMap Text Recipe

basicRecipes :: RecipeBook
basicRecipes = M.fromList
  [ ("iron-plate", recipe Smelting (singleItem "iron-ore") (singleItem "iron-plate") 192)
  , ("copper-plate", recipe Smelting (singleItem "copper-ore") (singleItem "copper-plate") 192)
  , ("iron-gear", recipe Standard (singleItem' "iron-plate" 2) (singleItem "iron-gear") 30)
  , ("red-science", recipe Standard (itemList [("copper-plate", 1), ("iron-gear", 1)]) (singleItem "red-science") 300)
  , ("copper-wire", recipe Standard (singleItem "copper-plate") (singleItem' "copper-wire" 2) 30)
  , ("green-circuit", recipe Standard (itemList [("copper-wire", 3), ("iron-plate", 1)]) (singleItem "green-circuit") 30)
  , ("yellow-belt", recipe Standard (itemList [("iron-plate", 1), ("iron-gear", 2)]) (singleItem' "yellow-belt" 2) 30)
  , ("inserter", recipe Standard (singleItems ["iron-plate", "iron-gear", "green-circuit"]) (singleItem "inserter") 30)
  , ("green-science", recipe Standard (singleItems ["yellow-belt", "inserter"]) (singleItem "green-science") 360)
  ]

type SpeedFactor = Ratio Int

data Assembler = Assembler RecipeType SpeedFactor

basicAssemblers :: HashMap Text Assembler
basicAssemblers = M.fromList
  [ ("assembler-1", Assembler Standard (1 % 2))
  , ("stone-furnace", Assembler Smelting (1 % 1))
  ]

electricDrills :: Int -> Int -- (ore / minute -> number of drills)
electricDrills orePerMinute = if r > 0 then q + 1 else q
  where
    ticksPerOre = 120 :: Int
    ticksPerMinute = 3600 :: Int
    (q, r) = quotRem (orePerMinute * ticksPerOre) ticksPerMinute

data FactorySchema = FactorySchema  { centralized :: [Either Text RecipeType]
                                    , recipeBook :: RecipeBook
                                    , assemblers :: HashMap Text Assembler
                                    }

instance Default FactorySchema where
  def = FactorySchema [Right Smelting, Left "iron-gear"] basicRecipes basicAssemblers

type CraftingBlock = (Text, Text, Int) -- assembler name, recipe name, count
type MiningBlock = (Text, Int) -- resource name, count

data FactoryF a = CraftingStage (NonEmpty CraftingBlock) a
                | MiningStage (NonEmpty MiningBlock)

type Factory = Fix FactoryF

instance Functor FactoryF where
  fmap f (CraftingStage cs x) = CraftingStage cs (f x)
  fmap _ (MiningStage ms) = MiningStage ms

-- production: items/min
type FactoryAlgebra = Production -> FactoryF Production

factoryAlgebra :: Member (Reader FactorySchema) r => Sem r FactoryAlgebra
factoryAlgebra = fmap buildStage ask
  where
    buildStage prod schema = let
      recipeProduction = 
      in MiningStage (("iron-ore", 3) :| [])


