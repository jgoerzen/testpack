{-
Copyright (C) 2004 - 2009 John Goerzen <jgoerzen@complete.org>

-}

{- |
   Module     : Test.QuickCheck.Utils
   Copyright  : Copyright (C) 2004-2005 John Goerzen
   License    : GNU LGPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Utilities for HUnit unit testing.

Written by John Goerzen, jgoerzen\@complete.org
-}

module Test.QuickCheck.Utils (-- * Comparisons
                              (@=?),
                              (@?=)
                              
                             )
where
import Test.QuickCheck

{- | Compare two values.  If same, the test passes.  If different, the result indicates
what was expected and what was received as part of the error. -}
(@=?) :: (Eq a, Show a) => a -> a -> Result
expected @=? actual = 
        Result {ok = Just (expected == actual), 
                arguments = ["Result: expected " ++ show expected ++ ", got " ++ show actual],
                stamp = []}
    
{- | Like '(@=?)', but with args in a different order. -}
(@?=) :: (Eq a, Show a) => a -> a -> Result
(@?=) = flip (@=?)

