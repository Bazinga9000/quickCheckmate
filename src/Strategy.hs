module Strategy where

import           Control.Lens
import           Control.Monad.State
import           History
import           Nudgeable
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen




--A strategy is a way of taking a list of past tests and their scores
--and getting a way to make the next test
newtype (Strategy a b) = MkStrategy {getStrategy :: History a b -> Gen a}


makeFromStrategy :: Strategy a b -> StateT (History a b) IO a
makeFromStrategy s = do
    hist <- get
    let gen = getStrategy s hist
    liftIO $ generate gen


--The simplest possible strategy: Do Not Care
stratBlind :: (Arbitrary a) => Strategy a b
stratBlind = MkStrategy $ const arbitrary

--Hill Climbing: Nudges the best test always. (Tests are always inserted best first)
stratHillClimbing :: (Arbitrary a, Nudgeable a) => Strategy a b
stratHillClimbing = MkStrategy $ \hist -> f $ hist ^. test_data where
    f []    = arbitrary
    f (h:t) = nudge $ h ^. test_case

--Genetic: Randomly picks a test to nudge with probability proportional to its score.
stratGenetic :: (Arbitrary a, Nudgeable a) => Strategy a Int
stratGenetic = MkStrategy $ \hist -> f $ hist ^. test_data where
    f [] = arbitrary
    f h = do
        let norm = 1 - minimum (map (^. score) h) --normalize minimum score to 1
        let rest = map (\x -> (x ^. score + norm, return $ x ^. test_case)) h
        --add possibility of arbitrary generation for "fail-safe"
        winner <- frequency $ (1, arbitrary):rest
        nudge winner
