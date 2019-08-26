module Data.Clamped (Clamped, clamped, mkClamped) where

import Control.Lens (Lens')

-- $setup
-- >>> import Control.Lens ((^.), (.~))
-- >>> import Data.Function ((&))

data Clamped a = Clamped a a a

-- | Lens into the clamped value. Writes through the lens are clamped.
--
-- prop> (lo :: Int) < hi ==> (mkClamped lo hi a & clamped .~ b) ^. clamped >= lo
-- prop> (lo :: Int) < hi ==> (mkClamped lo hi a & clamped .~ b) ^. clamped <= hi
clamped :: Ord a => Lens' (Clamped a) a
clamped f (Clamped lo hi a) = Clamped lo hi . clamp lo hi <$> f a

-- | Create a clamped value.
--
-- prop> (lo :: Int) < hi ==> mkClamped lo hi a ^. clamped >= lo
-- prop> (lo :: Int) < hi ==> mkClamped lo hi a ^. clamped <= hi
mkClamped :: Ord a => a -> a -> a -> Clamped a
mkClamped lo hi a = Clamped lo hi $ clamp lo hi a

clamp :: Ord a => a -> a -> a -> a
clamp lo hi a
  | a < lo = lo
  | a > hi = hi
  | otherwise = a
