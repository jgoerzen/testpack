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
                         runVerbTestText, runVerboseTests, qccheck, qctest,
                         qc2hu, tl)
    where
import Test.QuickCheck as QC
import Test.QuickCheck.Text
import Test.QuickCheck.Test
import Test.QuickCheck.Gen
import Test.QuickCheck.State
import qualified Test.QuickCheck.Property as P
import Test.QuickCheck.Property hiding (Result(reason))
import qualified Control.Exception
import qualified Test.HUnit as HU
#if MIN_VERSION_QuickCheck(2,7,0)
import Test.QuickCheck.Random (newQCGen, QCGen(..))
import System.Random (split)
#else
import System.Random (newStdGen, StdGen(..), split)
#define newStdGen newQCGen
#define StdGen QCGen
#endif
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
                    else HU.assertFailure $ msg ++ "\nReceived unexpected exception: "
                             ++ (show e) ++ "\ninstead of exception: " ++ (show selector)
        in do
           r <- Control.Exception.try action
           case r of
                  Left e -> thetest e
                  Right _ -> HU.assertFailure $ msg ++ "\nReceived no exception, but was expecting exception: " ++ (show selector)

mapassertEqual :: (Show b, Eq b) => String -> (a -> b) -> [(a, b)] -> [HU.Test]
mapassertEqual _ _ [] = []
mapassertEqual descrip func ((inp,result):xs) =
    (HU.TestCase $ HU.assertEqual descrip result (func inp)) : mapassertEqual descrip func xs

-- | qccheck turns the quickcheck test into an hunit test
qccheck :: (QC.Testable a) =>
           QC.Args -- ^ quickcheck config
        -> String -- ^ label for the property
        -> a      -- ^ quickcheck property
        -> HU.Test
qccheck config lbl property =
    HU.TestLabel lbl $ HU.TestCase $
      do result <- localquickCheckWithResult config property
         case result of
#if MIN_VERSION_QuickCheck(2,3,0)
           Success _ _ _ -> return ()
#else
           Success _ -> return ()
#endif
           _ -> HU.assertFailure (show result)

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

-- | qctest is equivalent to 'qccheck stdArgs'
qctest ::  (QC.Testable a) => String -> a -> HU.Test
qctest lbl = qccheck stdArgs lbl

{-
-- | modified version of the tests function from Test.QuickCheck
tests :: Args -> Gen Result -> QCGen -> Int -> Int -> [[String]] -> IO ()
tests config gen rnd0 ntest nfail stamps
  | ntest == maxSuccess config = return ()
  | nfail == maxDiscard config = assertFailure $ "Arguments exhausted after " ++ show ntest ++ " tests."
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
-}

{- | Convert QuickCheck tests to HUnit, with a configurable maximum test count.
Often used like this:

>q :: QC.Testable a => String -> a -> HU.Test
>q = qc2hu 250
>
>allt = [q "Int -> Integer" prop_int_to_integer,
>        q "Integer -> Int (safe bounds)" prop_integer_to_int_pass]
-}
qc2hu :: QC.Testable a => Int -> String -> a -> HU.Test
qc2hu maxTest = qccheck (stdArgs {maxSuccess = maxTest,
#if MIN_VERSION_QuickCheck(2,5,0)
                                  maxDiscardRatio = if maxTest /= 0 then 20000 `div` maxTest else 10
#else
                                  maxDiscard = 20000
#endif
                                 })

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
                HU.PutText putf st -> HU.PutText (myputf h putf) st
          myputf h putf x y z = do r <- putf x y z
                                   hFlush h
                                   return r

{- | Label the tests list.  See example under 'runVerboseTests'.-}
tl :: String -> [HU.Test] -> HU.Test
tl msg t = HU.TestLabel msg $ HU.TestList t

------------------------------------------------------------
-- below code lifted from quickcheck2, Tests.hs, and modified for this purpose

-- | Tests a property, using test arguments, produces a test result, and prints the results to 'stdout'.
localquickCheckWithResult :: Testable prop => Args -> prop -> IO Result
localquickCheckWithResult args p =
#if MIN_VERSION_QuickCheck(2,3,0)
#if MIN_VERSION_QuickCheck(2,6,0)
  (if chatty args then withStdioTerminal else withNullTerminal) $ \tm -> do
#else
  do
     tm  <- if chatty args then newStdioTerminal else newNullTerminal
#endif
#else
  do
     tm  <- newTerminal
#endif
     rnd <- case replay args of
              Nothing      -> newQCGen
              Just (rnd,_) -> return rnd
     test MkState{ terminal          = tm
                 , maxSuccessTests   = maxSuccess args
                 , maxDiscardedTests =
#if MIN_VERSION_QuickCheck(2,5,0)
                                       maxDiscardRatio args * maxSuccess args
#else
                                       maxDiscard args
#endif
                 , computeSize       = case replay args of
                                         Nothing    -> \n d -> (n * maxSize args)
                                                         `div` maxSuccess args
                                                             + (d `div` 10)
                                         Just (_,s) -> \_ _ -> s
                 , numSuccessTests   = 0
                 , numDiscardedTests = 0
                 , collected         = []
                 , expectedFailure   = False
                 , randomSeed        = rnd
#if !(MIN_VERSION_QuickCheck(2,3,0))
                 , isShrinking       = False
#endif
                 , numSuccessShrinks = 0
                 , numTryShrinks     = 0
#if MIN_VERSION_QuickCheck(2,7,0)
                 , numRecentlyDiscardedTests = 0
                 , numTotTryShrinks = 0
                 } (unGen (unProperty (property p)))
#else
                 } (unGen (property p))
#endif
  where 
--------------------------------------------------------------------------
-- main test loop
    test :: State -> (QCGen -> Int -> Prop) -> IO Result
    test st f
      | numSuccessTests st   >= maxSuccessTests st   = doneTesting st f
      | numDiscardedTests st >= maxDiscardedTests st = giveUp st f
      | otherwise                                    = runATest st f

    doneTesting :: State -> (QCGen -> Int -> Prop) -> IO Result
    doneTesting st f = 
      do
#if MIN_VERSION_QuickCheck(2,3,0)
        theOutput <- terminalOutput (terminal st)
#endif
        if expectedFailure st then
           return Success{ labels = summary st
#if MIN_VERSION_QuickCheck(2,3,0)
                         , numTests = numSuccessTests st
                         , output = theOutput 
#endif
                         }
           else
           return NoExpectedFailure{ labels = summary st
#if MIN_VERSION_QuickCheck(2,3,0)
                                   , numTests = numSuccessTests st
                                   , output = theOutput 
#endif
                                   }
  
    giveUp :: State -> (QCGen -> Int -> Prop) -> IO Result
    giveUp st f =
      do
#if MIN_VERSION_QuickCheck(2,3,0)
        theOutput <- terminalOutput (terminal st)
#endif
        return GaveUp{ numTests = numSuccessTests st
                     , labels   = summary st
#if MIN_VERSION_QuickCheck(2,3,0)
                     , output   = theOutput
#endif
                     }

    runATest :: State -> (QCGen -> Int -> Prop) -> IO Result
    runATest st f =
      do
        let size = computeSize st (numSuccessTests st) (numDiscardedTests st)
#if MIN_VERSION_QuickCheck(2,4,0)
        MkRose res ts <- protectRose (reduceRose (unProp (f rnd1 size)))
#elif MIN_VERSION_QuickCheck(2,3,0)
        (mres, ts) <- unpackRose (unProp (f rnd1 size))
        res <- mres
#elif MIN_VERSION_QuickCheck(2,1,0)
        MkRose mres ts <- protectRose (unProp (f rnd1 size))
        res <- mres
#endif
        callbackPostTest st res
     
        case ok res of
          Just True -> -- successful test
            do test st{ numSuccessTests = numSuccessTests st + 1
                      , randomSeed      = rnd2
                      , collected       = stamp res : collected st
                      , expectedFailure = expect res
                      } f
       
          Nothing -> -- discarded test
            do test st{ numDiscardedTests = numDiscardedTests st + 1
                      , randomSeed        = rnd2
                      , expectedFailure   = expect res
                      } f
         
          Just False -> -- failed test
            do 
#if MIN_VERSION_QuickCheck(2,3,0)
#if MIN_VERSION_QuickCheck(2,3,0)
               (numShrinks, totFailed, lastFailed) <- foundFailure st res ts
#else
               numShrinks <- foundFailure st res ts
#endif

               theOutput <- terminalOutput (terminal st)
#else
               foundFailure st res ts
#endif
               if not (expect res) then
                 return Success{ labels = summary st
#if MIN_VERSION_QuickCheck(2,3,0)
                               , numTests = numSuccessTests st+1
                               , output = theOutput
#endif 
                               }
                 else
                 return Failure{ usedSeed   = randomSeed st -- correct! (this will be split first)
                               , usedSize   = size
                               , reason     = P.reason res
                               , labels     = summary st
#if MIN_VERSION_QuickCheck(2,3,0)
                               , numTests   = numSuccessTests st + 1
                               , numShrinks = numShrinks
                               , output     = theOutput
#endif
#if MIN_VERSION_QuickCheck(2,7,0)
                               , numShrinkTries = totFailed
                               , numShrinkFinal = lastFailed
#endif
                               }
      where (rnd1,rnd2) = split (randomSeed st)
