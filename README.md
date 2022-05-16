# quickCheckmate

A prototype framework for targeted property-based testing, written as a CMSC488B final project at the University of Maryland.

`stack run` will simply run two tests set up using the framework, followed by a quickCheck test verifying part of the code

The first test checks the property that all integers are at most 20, using the Hill Climbing strategy (take the best scoring test so far and nudge it). Note that this may occasionally fail to produce a failing test case as the default implementation for nudging Integers only shifts them by up to 2 in either direction and may be unlucky enough to reach the goal within 100 cases.

The second test checks a property of a very simple tree structure:
```
newtype Rose = Rose [Rose]
```
namely, that all Roses are no more than five layers deep. This test uses the Genetic strategy (choose a random test case weighted by score, and nudge it), and uses a utility function that is not the identity (in particular, a test case will score high if it has many nodes regardless of depth). This test serves to demonstrate the Genetic strategy and that utility functions not directly related to the property can still produce effective results. This test is allowed to run infinitely to show that results can be produces.

This project can be hosted on the class website.
