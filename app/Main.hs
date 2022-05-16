{-# LANGUAGE FlexibleInstances #-}
import           Control.Monad
import           Nudgeable
import           QuickCheckmate
import           Strategy
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen
import           Tests

--extremely simple example
prop_isAtMost20 :: Int -> Bool
prop_isAtMost20 x = x <= 20

util_isAtMost20 :: Int -> Int
util_isAtMost20 = id

{-
less simple example where the utility function and the thing that is to be
tested are different
-}
newtype Rose = Rose [Rose] deriving Show

height :: Rose -> Int
height (Rose []) = 0
height (Rose s)  = 1 + maximum (map height s)

breadth :: Rose -> Int
breadth (Rose l) = sum $ map breadth l
instance Arbitrary Rose where
    arbitrary = frequency [(7, return $ Rose []), (3, bigRose)] where
        bigRose = do
            size <- elements [1,1,2,2,2,3]
            Rose <$> replicateM size arbitrary

instance Nudgeable Rose where
    nudge r = frequency [(3, lopRose r), (3, addRose r), (3, mutateRose r)] where
        lopRose (Rose [])    = return $ Rose []
        lopRose (Rose (h:t)) = return $ Rose t
        addRose (Rose l) = Rose . (:l) <$> arbitrary
        mutateRose (Rose l) = Rose <$> mapM nudge l

prop_smallRose :: Rose -> Bool
prop_smallRose r = height r <= 5

--wide trees are likely to be tall
util_smallRose :: Rose -> Int
util_smallRose = breadth


main :: IO ()
main = do
    putStrLn "Integer Test (All Integers are less than 20)"
    quickCheckmate (stratHillClimbing :: Strategy Int Int) prop_isAtMost20 util_isAtMost20
    putStrLn "Rose Test (All Roses have height at most 5)"
    quickCheckmateInf (stratGenetic :: Strategy Rose Int) prop_smallRose util_smallRose
    putStrLn "QuickCheck Test of History Sorting"
    quickCheck prop_addKeepsSorted
    return ()
