{- |
   Module     : Test.HUnit.Tools
   Copyright  : Copyright (C) 2004-2010 John Goerzen
   License    : GNU LGPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Utilities for HUnit unit testing.

Written by John Goerzen, jgoerzen\@complete.org
-}

module Test.HUnit.Tools (assertRaises, mapassertEqual, 
                         runVerbTestText, runVerboseTests,
                         tl)
    where
import Test.HUnit
import qualified Control.Exception
import qualified Test.HUnit as HU
import System.Random
import System.IO
import Text.Printf

{- | Asserts that a specific exception is raised by a given action. -}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 610
assertRaises :: (Show a, Control.Exception.Exception e, Show e, Eq e) =>
                String -> e -> IO a -> IO ()
#else
assertRaises :: Show a => String -> Control.Exception.Exception -> IO a -> IO ()
#endif
assertRaises msg selector action =
    let thetest e = if e == selector then return ()
                    else assertFailure $ msg ++ "\nReceived unexpected exception: "
                             ++ (show e) ++ "\ninstead of exception: " ++ (show selector)
        in do
           r <- Control.Exception.try action
           case r of
                  Left e -> thetest e
                  Right _ -> assertFailure $ msg ++ "\nReceived no exception, but was expecting exception: " ++ (show selector)

mapassertEqual :: (Show b, Eq b) => String -> (a -> b) -> [(a, b)] -> [Test]
mapassertEqual _ _ [] = []
mapassertEqual descrip func ((inp,result):xs) =
    (TestCase $ assertEqual descrip result (func inp)) : mapassertEqual descrip func xs


-- Modified from HUnit
{- | Like 'runTestText', but with more verbose output. -}
runVerbTestText :: HU.PutText st -> HU.Test -> IO (HU.Counts, st)
runVerbTestText (HU.PutText put us) t = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  (counts, us') <- HU.performTest reportStart reportError reportFailure us t
  us'' <- put (HU.showCounts counts) True us'
  return (counts, us'')
 where
  reportStart ss us = do hFlush stderr
                         hPrintf stdout "\rTesting %-70s\n"
                                     (HU.showPath (HU.path ss))
                         hFlush stdout
                         put (HU.showCounts (HU.counts ss)) False us
  reportError   = reportProblem "Error:"   "Error in:   "
  reportFailure = reportProblem "Failure:" "Failure in: "
  reportProblem p0 p1 msg ss us = put line True us
   where line  = "### " ++ kind ++ path' ++ '\n' : msg
         kind  = if null path' then p0 else p1
         path' = HU.showPath (HU.path ss)


{- | Run verbose tests.  Example:

>test1 = TestCase ("x" @=? "x")
>
>alltests = [TestLabel "test1" test1,
>            tl "TestNum" TestNum.allt,
>            tl "TestMap" TestMap.allt,
>            tl "TestTime" TestTime.allt]
>
>main = do runVerboseTests (TestList alltests)
>          return ()
-}
runVerboseTests :: HU.Test -> IO (HU.Counts, Int)
runVerboseTests tests =
    -- runVerbTestText (HU.putTextToHandle stderr True) $ tests
    runVerbTestText (myPutText stderr True) $ tests
    where myPutText h b = 
              case HU.putTextToHandle h b of
                PutText putf st -> PutText (myputf h putf) st
          myputf h putf x y z = do r <- putf x y z
                                   hFlush h
                                   return r

{- | Label the tests list.  See example under 'runVerboseTests'.-}
tl :: String -> [Test] -> Test
tl msg t = HU.TestLabel msg $ HU.TestList t
