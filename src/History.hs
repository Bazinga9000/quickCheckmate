{-# LANGUAGE TemplateHaskell #-}

module History where

import           Control.Lens
import           Control.Lens.TH


--A TestResult is the data regarding a single (non-discarded) test that was executed
data (TestResult a u) = MkResult {
    _test_case   :: a,
    _score       :: u,
    _total_index :: Int,
    _kept_index  :: Int
} deriving Show

--A History is a list of past test cases and their corresponding scores
data (History a u) = MkHistory {
    _total_tests :: Int,
    _kept_tests  :: Int,
    _test_data   :: [TestResult a u]
} deriving Show

makeLenses ''TestResult
makeLenses ''History

emptyHistory :: History a u
emptyHistory = MkHistory {_total_tests = 0, _kept_tests = 0, _test_data = []}

addDiscardedTest :: History a u -> History a u
addDiscardedTest = over total_tests (+1)

makeKeptResult :: a -> u -> History a u -> TestResult a u
makeKeptResult tcase tscore hist = MkResult {
        _test_case = tcase,
        _score = tscore,
        _total_index = hist ^. total_tests,
        _kept_index = hist ^. kept_tests
    }


insertInto :: Ord u => TestResult a u -> [TestResult a u] -> [TestResult a u]
insertInto r [] = [r]
insertInto r (x:xs)
    | r ^. score >= x ^. score = r : x : xs
    | otherwise = x : insertInto r xs

addKeptTest :: Ord u => a -> u -> History a u -> History a u
addKeptTest tcase tscore hist = hist &~ do
    let res = makeKeptResult tcase tscore hist
    test_data .= insertInto res (hist ^. test_data)
    kept_tests += 1
