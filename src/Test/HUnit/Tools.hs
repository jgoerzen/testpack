{- |
   Module     : Test.HUnit.Tools
   Copyright  : Copyright (C) 2004-2009 John Goerzen
   License    : GNU LGPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Utilities for HUnit unit testing.

Written by John Goerzen, jgoerzen\@complete.org
-}

module Test.HUnit.Tools (assertRaises, mapassertEqual, 
                         runVerbTestText, runVerboseTests, qccheck, qctest,
                         qc2hu, qc2huVerbose, tl)
    where
import Test.HUnit
import Test.QuickCheck as QC
import qualified Control.Exception
import qualified Test.HUnit as HU
import System.Random
import System.IO
import Text.Printf

{- | Asserts that a specific exception is raised by a given action. -}
assertRaises :: Show a => String -> Control.Exception.Exception -> IO a -> IO ()
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

-- | qccheck turns the quickcheck test into an hunit test
qccheck :: (QC.Testable a) =>
           QC.Config -- ^ quickcheck config
        -> String -- ^ label for the property
        -> a      -- ^ quickcheck property
        -> Test
qccheck config lbl property =
    TestLabel lbl $ TestCase $
              do rnd <- newStdGen
                 tests config (evaluate property) rnd 0 0 []


-- Modified from HUnit
{- | Like 'runTestText', but with more verbose output. -}
runVerbTestText :: HU.PutText st -> HU.Test -> IO (HU.Counts, st)
runVerbTestText (HU.PutText put us) t = do
  (counts, us') <- HU.performTest reportStart reportError reportFailure us t
  us'' <- put (HU.showCounts counts) True us'
  return (counts, us'')
 where
  reportStart ss us = do hPrintf stdout "\rTesting %-70s\n"
                                     (HU.showPath (HU.path ss))
                         hFlush stdout
                         put (HU.showCounts (HU.counts ss)) False us
  reportError   = reportProblem "Error:"   "Error in:   "
  reportFailure = reportProblem "Failure:" "Failure in: "
  reportProblem p0 p1 msg ss us = put line True us
   where line  = "### " ++ kind ++ path' ++ '\n' : msg
         kind  = if null path' then p0 else p1
         path' = HU.showPath (HU.path ss)

-- | qctest is equivalent to 'qccheck defaultConfig'
qctest ::  (QC.Testable a) => String -> a -> Test
qctest lbl = qccheck defaultConfig lbl

-- | modified version of the tests function from Test.QuickCheck
tests :: Config -> Gen Result -> StdGen -> Int -> Int -> [[String]] -> IO ()
tests config gen rnd0 ntest nfail stamps
  | ntest == configMaxTest config = return ()
  | nfail == configMaxFail config = assertFailure $ "Arguments exhausted after " ++ show ntest ++ " tests."
  | otherwise               =
      do putStr (configEvery config ntest (arguments result))
         case ok result of
           Nothing    ->
             tests config gen rnd1 ntest (nfail+1) stamps
           Just True  ->
             tests config gen rnd1 (ntest+1) nfail (stamp result:stamps)
           Just False ->
             assertFailure $  ( "Falsifiable, after "
                   ++ show ntest
                   ++ " tests:\n"
                   ++ unlines (arguments result)
                    )
     where
      result      = generate (configSize config ntest) rnd2 gen
      (rnd1,rnd2) = split rnd0

{- | Convert QuickCheck tests to HUnit, with a configurable maximum test count,
and running counter on the screen for long-running tests.  Often used like this:

>q :: QC.Testable a => String -> a -> HU.Test
>q = qc2hu 250
>
>allt = [q "Int -> Integer" prop_int_to_integer,
>        q "Integer -> Int (safe bounds)" prop_integer_to_int_pass]
-}
qc2hu :: QC.Testable a => Int -> String -> a -> HU.Test
qc2hu maxTest = qccheck (defaultConfig {configMaxTest = maxTest, configMaxFail = 20000,
                            configEvery = testCount})
    -- configEvery = testCount for displaying a running test counter
    where testCountBase n = " (test " ++ show n ++ "/" ++ show maxTest ++ ")"
          testCount n _ = testCountBase n ++ 
                          replicate (length (testCountBase n)) '\b'

{- | Like 'qc2hu', but show the test itself for each one. -}
qc2huVerbose :: QC.Testable a => Int -> String -> a -> HU.Test
qc2huVerbose maxTest = 
    qccheck (defaultConfig {configMaxTest = 250, configMaxFail = 20000,
                            configEvery = \n args -> show n ++ ":\n" ++ unlines args})

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
    runVerbTestText (HU.putTextToHandle stderr True) $ tests

{- | Label the tests list.  See example under 'runVerboseTests'.-}
tl :: String -> [Test] -> Test
tl msg t = HU.TestLabel msg $ HU.TestList t
