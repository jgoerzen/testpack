{-
Copyright (C) 2004 - 2009 John Goerzen <jgoerzen@complete.org>

-}

{- |
   Module     : Test.QuickCheck.Instances
   Copyright  : Copyright (C) 2004-2005 John Goerzen
   License    : GNU LGPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Utilities for HUnit unit testing.

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
    coarbitrary = coarbitrary . Map.keys

instance Arbitrary Word8 where
    arbitrary = sized $ \n -> choose (0, min (fromIntegral n) maxBound)
    coarbitrary n = variant (if n >= 0 then 2 * x else 2 * x + 1)
                where x = abs . fromIntegral $ n

instance Random Word8 where
    randomR (a, b) g = (\(x, y) -> (fromInteger x, y)) $
                       randomR (toInteger a, toInteger b) g
    random g = randomR (minBound, maxBound) g

instance Arbitrary Char where
    arbitrary = sized $ \n -> choose ('\NUL', '\xFF')
    coarbitrary n = variant (toEnum (2 * x + 1))
                where x = (abs . fromEnum $ n)::Int

