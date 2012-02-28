{-
Copyright (C) 2004 - 2010 John Goerzen <jgoerzen@complete.org>

-}

{- |
   Module     : Test.QuickCheck.Instances
   Copyright  : Copyright (C) 2004-2010 John Goerzen
   License    : GNU LGPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Provides Arbitrary instances for:

* Map.Map k v

* Word8 (also a Random instance)

Written by John Goerzen, jgoerzen\@complete.org
-}

module Test.QuickCheck.Instances where
import Test.QuickCheck
import System.Random
import qualified Data.Map as Map
import Data.Word


instance (Arbitrary k, Arbitrary v, Eq k, Ord k) => Arbitrary (Map.Map k v) where
    arbitrary = 
        do items <- arbitrary
           return $ Map.fromList items

instance (CoArbitrary k, CoArbitrary v, Eq k, Ord k) => CoArbitrary (Map.Map k v) where
    coarbitrary = coarbitrary . Map.keys

#if MIN_VERSION_QuickCheck(2,3,0)
    -- we have Word8 instances here
#else
instance Arbitrary Word8 where
    arbitrary = sized $ \n -> choose (0, min (fromIntegral n) maxBound)

instance CoArbitrary Word8 where
    coarbitrary n = variant (if n >= 0 then 2 * x else 2 * x + 1)
                where x = abs . fromIntegral $ n
#endif

#if !MIN_VERSION_random(1,0,1)
instance Random Word8 where
    randomR (a, b) g = (\(x, y) -> (fromInteger x, y)) $
                       randomR (toInteger a, toInteger b) g
    random g = randomR (minBound, maxBound) g
#endif
