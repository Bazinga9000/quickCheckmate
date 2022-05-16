module Tests where

import           Control.Lens
import           Control.Monad
import           Data.List
import           Data.Ord
import           History
import           Test.QuickCheck

{-
I have no idea how the monad transformer stacks that operate this framework can
be tested with quickCheck, so I have instead opted to write a test for a simpler
yet still essential part of the system. Since all strategies expect the list of
test cases to be in order with highest score first, we check that adding a new
test case preserves sorted order of the test cases
-}

toTestResult :: (a,b) -> Int -> TestResult a b
toTestResult (a,b) n = MkResult {
    _test_case = a,
    _score = b,
    _total_index = n,
    _kept_index = n
}

instance (Arbitrary a, Arbitrary b, Ord b) => Arbitrary (History a b) where
    arbitrary = do
        size <- choose (1,100)
        casetuples <- replicateM size arbitrary
        let cases = zipWith toTestResult casetuples [1..size]
        let sortedCases = sortOn (Down . (^. score)) cases
        return MkHistory {
            _total_tests = size,
            _kept_tests = size,
            _test_data = sortedCases
        }


prop_isSorted :: Ord b => [TestResult a b] -> Bool
prop_isSorted [] = True
prop_isSorted [a] = True
prop_isSorted (a:b:xs) = sa >= sb && prop_isSorted (b:xs) where
        sa = a ^. score
        sb = b ^. score

prop_historyIsSorted :: Ord b => History a b -> Bool
prop_historyIsSorted hist = prop_isSorted $ hist ^. test_data

prop_addKeepsSorted :: Int -> Int -> History Int Int -> Bool
prop_addKeepsSorted a s hist = prop_historyIsSorted $ addKeptTest a s hist
