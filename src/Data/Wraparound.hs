module Data.Wraparound where

import Control.Lens (Lens')

-- $setup
-- >>> import Control.Lens ((^.), (.~))
-- >>> import Data.Function ((&))
-- >>> import Test.QuickCheck

-- | Like a 'Data.Clamped.Clamped' value, but if you go off either end
-- you wrap around and come in on the other side.
data Wraparound a = Wraparound a a a

-- | Lens into the wrapping value. Writes through the lens wrap around.
--
-- >>> :{
--   quickCheck $ \lo hi a b n -> (lo :: Int) < hi
--     ==> (mkWraparound lo hi a & wraparound .~ b) ^. wraparound
--     === (mkWraparound lo hi a & wraparound .~ (b + n * (hi - lo))) ^. wraparound
-- :}
-- +++ OK, passed 100 tests...
wraparound :: (Num a, Ord a) => Lens' (Wraparound a) a
wraparound f (Wraparound lo hi a) = Wraparound lo hi . wrap lo hi <$> f a

-- | Create a wrapping value.
--
-- Wraparounds are bounded by @lo@ and @hi@, just like 'Data.Clamped.Clamped':
-- prop> (lo :: Int) < hi ==> mkWraparound lo hi a ^. wraparound >= lo
-- prop> (lo :: Int) < hi ==> mkWraparound lo hi a ^. wraparound < hi
--
-- But they also loop around:
-- >>> :{
--   quickCheck $ \lo hi a n -> (lo :: Int) < hi
--     ==> mkWraparound lo hi a ^. wraparound
--     === mkWraparound lo hi (a + n * (hi - lo)) ^. wraparound
-- :}
-- +++ OK, passed 100 tests...
mkWraparound :: (Num a, Ord a) => a -> a -> a -> Wraparound a
mkWraparound lo hi a = Wraparound lo hi $ wrap lo hi a

wrap :: (Num a, Ord a) => a -> a -> a -> a
wrap lo hi a
  | a < lo = wrap lo hi $ a + d
  | a >= hi = wrap lo hi $ a - d
  | otherwise = a
  where
    d = hi - lo
