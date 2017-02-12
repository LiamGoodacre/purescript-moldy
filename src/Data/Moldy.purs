module Data.Moldy where

import Prelude
import Data.Foldable (class Foldable, foldMap, foldr, foldl, foldrDefault, foldlDefault)
import Data.String (toCharArray)
import Data.Monoid (class Monoid, mempty)
import Data.Monoid.Endo (Endo(..))
import Data.Monoid.Dual (Dual(..))
import Data.Newtype (unwrap)

-- | A Moldable type `t` is a monomorphic foldable structure with
-- | elements of type `e`.
class Moldable t e | t -> e where
  moldMap :: forall m. Monoid m => (e -> m) -> t -> m
  moldl :: forall m. (m -> e -> m) -> m -> t -> m
  moldr :: forall m. (e -> m -> m) -> m -> t -> m

-- | A default implementation of `moldMap` based on `moldr`
moldMapDefaultR :: forall t e m. (Moldable t e, Monoid m) =>
  (e -> m) -> t -> m
moldMapDefaultR f t = moldr (append <<< f) mempty t

moldMapDefaultL :: forall t e m. (Moldable t e, Monoid m) =>
  (e -> m) -> t -> m
moldMapDefaultL f t = moldl (\m -> append m <<< f) mempty t

-- | A default implementation of `moldl` based on `moldMap`
moldlDefault :: forall t e m. Moldable t e =>
  (m -> e -> m) -> m -> t -> m
moldlDefault f u t = unwrap (unwrap (moldMap (Dual <<< Endo <<< flip f) t)) u

-- | A default implementation of `moldr` based on `moldMap`
moldrDefault :: forall t e m. Moldable t e =>
  (e -> m -> m) -> m -> t -> m
moldrDefault f u t = unwrap (moldMap (Endo <<< f) t) u

-- | Combine all the elements in the moldable type using the action of the monoid
mold :: forall t e. (Moldable t e, Monoid e) => t -> e
mold = moldMap id

-- | Every Foldable is Moldable
instance moldableFoldable :: Foldable t => Moldable (t e) e where
  moldMap = foldMap
  moldl = foldl
  moldr = foldr

-- TODO: make more efficient
instance moldableString :: Moldable String Char where
  moldMap f = foldMap f <<< toCharArray
  moldl f = moldlDefault f
  moldr f = moldrDefault f
  
-- | For any `Moldable t e`, `Mold t e` is Foldable
data Moldy t e a = Moldy (e -> a) t

-- TODO: other instances? Profunctor, etc?
derive instance functorMoldy :: Functor (Moldy t e)

-- | Construct a Moldy
moldy :: forall t e. t -> Moldy t e e
moldy = Moldy id

instance foldableMoldy :: Moldable t e => Foldable (Moldy t e) where
  foldMap f (Moldy g m) = moldMap (f <<< g) m
  foldr f = foldrDefault f
  foldl f = foldlDefault f

