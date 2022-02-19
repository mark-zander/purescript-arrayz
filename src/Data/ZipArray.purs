module Data.ZipArray
  ( ZipArray(..)
  )
  where

import Prelude

import Control.Plus (class Alt, class Plus)
import Data.Array as Array
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Newtype (class Newtype)
import Data.Traversable (class Foldable, class Traversable)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Test.QuickCheck.Arbitrary (class Arbitrary)

-- ZipArray

newtype ZipArray a = ZipArray (Array a)

instance showZipArray :: Show a => Show (ZipArray a) where
  show (ZipArray xs) = "(ZipArray " <> show xs <> ")"

derive instance newtypeZipArray :: Newtype (ZipArray a) _

derive newtype instance arbZipArray ::
  (Arbitrary a) => Arbitrary (ZipArray a)

derive instance eqZipArray :: Eq a => Eq (ZipArray a)

derive instance ordZipArray :: Ord a => Ord (ZipArray a)

derive newtype instance semigroupZipArray :: Semigroup (ZipArray a)

derive newtype instance monoidZipArray :: Monoid (ZipArray a)

derive newtype instance foldableZipArray :: Foldable ZipArray

derive newtype instance foldableWithIndexZipArray ::
  FoldableWithIndex Int ZipArray

derive newtype instance traversableZipArray :: Traversable ZipArray

derive newtype instance traversableWithIndexZipArray ::
  TraversableWithIndex Int ZipArray

derive newtype instance functorZipArray :: Functor ZipArray

derive newtype instance functorWithIndexZipArray ::
  FunctorWithIndex Int ZipArray

instance applyZipArray :: Apply ZipArray where
  apply (ZipArray fs) (ZipArray xs) =
    ZipArray (Array.zipWith ($) fs xs)

-- No implementation of pure.
-- instance applicativeZipArray :: Applicative ZipArray where
--   pure = ZipArray <<< repeat

instance altZipArray :: Alt ZipArray where
  alt (ZipArray xs) (ZipArray ys) =
    ZipArray $ xs <> Array.drop (Array.length xs) ys

instance plusZipArray :: Plus ZipArray where
  empty = mempty

-- Requires Applicative/pure
-- instance alternativeZipArray :: Alternative ZipArray
