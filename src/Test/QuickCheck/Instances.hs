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

