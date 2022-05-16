module Nudgeable(
    Nudgeable, nudge
) where

import           Control.Monad
import           Test.QuickCheck.Gen


--The Nudgeable class, defining a way to get a new piece of data "similar" to a previous one.
class Nudgeable a where
    nudge :: a -> Gen a

instance (Nudgeable a, Nudgeable b) => Nudgeable (a,b) where
    nudge (a,b) = liftM2 (,) (nudge a) (nudge b)

instance (Nudgeable a, Nudgeable b, Nudgeable c) => Nudgeable (a,b,c) where
    nudge (a,b,c) = liftM3 (,,) (nudge a) (nudge b) (nudge c)

instance (Nudgeable a) => Nudgeable [a] where
    nudge = mapM (\a -> oneof [return a, nudge a])

instance Nudgeable Int where
    nudge x = (+x) <$> elements [-2,-1,-1,0,0,0,1,1,2]

--shift x by normal distribution w/ mean 0 and stdev 1, using Box-Muller Transform
instance Nudgeable Double where
    nudge x = do
        y1 <- choose (0.0,1.0)
        y2 <- choose (0.0,1.0)
        let z1 = cos (2 * pi * y2) * sqrt (-2 * log y1)
        let z2 = sin (2 * pi * y2) * sqrt (-2 * log y1)
        elements [x + z1, x + z2]
