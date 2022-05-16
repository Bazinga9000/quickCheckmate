{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module QuickCheckmate where

import           Control.Lens
import           Control.Monad.State
import           History
import           Strategizable
import           Strategy
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen

getStr :: (Show a, Show u) => (a, Result, u) -> String
getStr (a, _, _) = show a
{-
getStr (a, Discarded, _) = "Test Case " ++ show a ++ " discarded, "
getStr (a, Checked False, _) = "Test Case " ++ show a ++ " failed!"
getStr (a, Checked True, s) = "Test Case " ++ show a ++ " passed, score " ++ show s ++ "."
-}

doTestNTimes :: (Show a, Show u, Ord u, Strategizable a p s u) =>
    Int -> Strategy a u -> p -> s -> StateT (History a u) IO ()
doTestNTimes 0 _ _ _ = liftIO $ putStrLn "OK, No failure found."
doTestNTimes n strat prop score = do
    testResult <- strategize strat prop score
    hist <- get
    case testResult of
        (a, Discarded, _) -> do
            --liftIO $ putStrLn $ getStr testResult
            put $ addDiscardedTest hist
            doTestNTimes n strat prop score
        (a, Checked False, _) -> do
                liftIO $ putStrLn $ "--Failure!--"
                liftIO $ putStrLn $ getStr testResult
        (a, Checked True, s) -> do
            put $ addKeptTest a s hist
            doTestNTimes (n-1) strat prop score
            --liftIO $ putStrLn $ getStr testResult -- ++ " " ++ show n ++ " remain."


quickCheckmateN :: (Show a, Show u, Ord u, Strategizable a p s u) =>
    Int -> Strategy a u -> p -> s -> IO ((), History a u)
quickCheckmateN n strat prop score = runStateT (doTestNTimes n strat prop score) emptyHistory

quickCheckmate :: (Show a, Show u, Ord u, Strategizable a p s u) =>
    Strategy a u -> p -> s -> IO ((), History a u)
quickCheckmate = quickCheckmateN 100

quickCheckmateInf :: (Show a, Show u, Ord u, Strategizable a p s u) =>
    Strategy a u -> p -> s -> IO ((), History a u)
quickCheckmateInf = quickCheckmateN (-1)
