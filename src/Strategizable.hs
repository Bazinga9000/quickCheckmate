{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Strategizable where

import           Control.Lens
import           Control.Monad.State
import           History
import           Strategy
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen

data Result = Checked Bool | Discarded

class Reducible a where
    reduce :: a -> Result

instance Reducible Bool where
    reduce = Checked

instance Reducible Result where
    reduce = id

{-
a: The type that the strategy generates (for functions of multiple arguments it makes a tuple)
s: the scorer (a -> ... -> u)
p: the property (Reducible r => a -> ... -> r)
u: the score type
-}
class Strategizable a p s u where
    strategize :: Strategy a u -> p -> s ->
        StateT (History a u) IO (a, Result, u)

{-
The reason why there's no fully inductive Strategizable definition
is because the Strategy needs to generate all the arguments at
once, since the score depends on all of them. I couldn't figure
out a way to make the types work out with an implication like
Strategizable b p s u => Strategizable (a,b) (a -> p) (a -> s) u

So, instead, I took a page from (,,,,,,,,,,,)'s book and provided
a handful of tuple defintions already
-}
instance Reducible r => Strategizable a (a -> r) (a -> u) u where
    strategize strat prop scorer = do
        x <- makeFromStrategy strat
        return (x, reduce $ prop x, scorer x)

instance Reducible r => Strategizable (a,b) (a -> b -> r) (a -> b -> u) u where
    strategize strat prop scorer = do
        x@(a,b) <- makeFromStrategy strat
        return (x, reduce $ prop a b, scorer a b)

instance Reducible r => Strategizable (a,b,c) (a -> b -> c -> r) (a -> b -> c -> u) u where
    strategize strat prop scorer = do
        x@(a,b,c) <- makeFromStrategy strat
        return (x, reduce $ prop a b c, scorer a b c)

{-
However, we *can* induct if we don't care about a strategy for the new arguments.
That is, if they're generated completely randomly. So, we have two different
partially inductive instances, one for when the additional blindly generated
parameter is relevant to the score, and one for when it is not. Note that this
does *NOT* store such arguments, meaning you cannot find them later.
-}
{-
instance (Arbitrary a, Strategizable b p s u) => Strategizable b (a -> p) (a -> s) u where
    strategize strat prop scorer = do
        new <- liftIO $ generate arbitrary
        strategize strat (prop new) (scorer new)
-}
