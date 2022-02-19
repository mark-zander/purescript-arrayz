module Test.Main where

import Prelude

import Effect (Effect)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Generic.Rep (class Generic)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum.Generic (genericCardinality,
    genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Show.Generic (genericShow)
import Test.QuickCheck.Arbitrary
  (class Coarbitrary, genericCoarbitrary)
import Test.QuickCheck.Laws (checkLaws)
import Test.QuickCheck.Laws.Data as Data
import Test.QuickCheck.Laws.Control as Control
import Type.Proxy (Proxy(..), Proxy2(..))

import Data.ZipArray (ZipArray)
import Data.ArrayIx (ArrayIx)

prxZipArray = Proxy :: Proxy (ZipArray Int)
prx2ZipArray = Proxy2 :: Proxy2 ZipArray

main :: Effect Unit
main = do
  checkLaws "ZipArray" do
    Data.checkEq prxZipArray
    Data.checkOrd prxZipArray
    Data.checkSemigroup prxZipArray
    Data.checkMonoid prxZipArray
    Data.checkFoldable prx2ZipArray
    Data.checkFunctor prx2ZipArray
    Data.checkFunctorWithIndex prx2ZipArray
    Control.checkApply prx2ZipArray
    Control.checkAlt prx2ZipArray
    Control.checkPlus prx2ZipArray

  checkLaws "ArrayIx" do
    Data.checkEq prxArrayIx
    Data.checkOrd prxArrayIx
    -- Data.checkSemigroup prxArrayIx
    -- Data.checkMonoid prxArrayIx
    Data.checkFoldable prx2ArrayIx
    Data.checkFunctor prx2ArrayIx
    Data.checkFunctorWithIndex prx2ArrayIx
    Control.checkApply prx2ArrayIx
    -- Control.checkAlt prx2ArrayIx
    -- Control.checkPlus prx2ArrayIx

prxArrayIx = Proxy :: Proxy (ArrayIx NN12 Int)
prx2ArrayIx = Proxy2 :: Proxy2 (ArrayIx NN12)

data NN12 = N0 | N1 |  N2 |  N3 |  N4 |  N5 |  N6 |  N7 |  N8 |  N9
  | N10 | N11

instance coarbitraryNN12 ::
        (Generic NN12 rep, Coarbitrary rep) =>
        Coarbitrary NN12 where
    coarbitrary = genericCoarbitrary
derive instance eqNN12 :: Eq NN12
derive instance ordNN12 :: Ord NN12
derive instance genericNN12 :: Generic NN12 _
instance showNN12 :: Show NN12 where
  show = genericShow
instance boundedNN12 :: Bounded NN12 where
  bottom = genericBottom   
  top = genericTop
instance enumNN12 :: Enum NN12 where
  succ = genericSucc
  pred = genericPred
instance boundedenumNN12 :: BoundedEnum NN12 where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

