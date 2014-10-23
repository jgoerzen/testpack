{-
Copyright (C) 2004 - 2009 John Goerzen <jgoerzen@complete.org>

-}

{- |
   Module     : Test.QuickCheck.Tools
   Copyright  : Copyright (C) 2004-2005 John Goerzen
   License    : GNU LGPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Utilities for HUnit unit testing.

Written by John Goerzen, jgoerzen\@complete.org
-}

module Test.QuickCheck.Tools (-- * Comparisons
                              (@=?),
                              (@?=)
                              
                             )
where
#if MIN_VERSION_QuickCheck(2,6,0)
import Test.QuickCheck.Property (Result(..), callbacks, expect, theException, ok, reason, stamp)
#if MIN_VERSION_QuickCheck(2,7,0)
#else
import Test.QuickCheck.Property (Result(..), callbacks, expect, interrupted, ok, reason, stamp)
#endif
#else
import Test.QuickCheck hiding (Result, reason)
import Test.QuickCheck.Property
#endif

{- | Compare two values.  If same, the test passes.  If different, the result indicates
what was expected and what was received as part of the error. -}
(@=?) :: (Eq a, Show a) => a -> a -> Result
expected @=? actual = 
        MkResult {ok = Just (expected == actual), 
#if MIN_VERSION_QuickCheck(2,7,0)
                  expect = True, theException = Nothing,
#else
                  expect = True, interrupted = False,
#endif
                  reason = "Result: expected " ++ show expected ++ ", got " ++ show actual,
                  stamp = [], callbacks = []}
    
{- | Like '@=?', but with args in a different order. -}
(@?=) :: (Eq a, Show a) => a -> a -> Result
(@?=) = flip (@=?)

