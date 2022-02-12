# `apropos-tx`
Lets you write more tests than you write lines of code.

## What is this?
It's a thing that generates property tests for you from a model.

### What kind of model?
A logical model that describes the device you are testing.

### What device?
This is the thing you are hooking your test equipment up to.

### What?
It's a function.

### And how do you write this logical model?
Good question. First you start with a model. Then you describe it's properties.

### What's a model?
It's any datatype of your choosing... For example it could be this thing

```Haskell
instance Proper MyModel where
  data Model MyModel =
    This | That | TheOther { things :: [Thing] }
```

### And the properties?
They are an enum like this.

```Haskell
instance Proper MyModel where
  data Property MyModel =
      DoesThis
    | DoesThat
    | DoesTheNextThing
    deriving (Bounded,Eq,Ord,Enum,Show)

```

It satisfies the constraints required for these terms to be used as Propositions in a Propositional Calculus.

This means we can write logical statements in terms of them.

For Example.

```Haskell
term = (Var DoesThis :->: Var DoesThat)
    :&&: (Var DoesThat :->: Var DoesTheNextThing)

```

Neat! Terms like these are used to encode Three aspects of our model.
1. Model Internal Logic
2. Expected Device Response
3. Generator Transformation Matches

#### Model Internal Logic
This is a term you define to constrain which properties of the model can be true at the same time.

```Haskell
instance Proper MyModel where
  logic = Var This :->: Var That
```

For example if This implies That then whenever This is true That must also be true.

#### When we expect the device under test to PASS or FAIL
This tells the test runner when to expect Passing/Failing of the device.

```Haskell
instance Proper MyModel where
  expect = Var VALIDATES

```

You could use the idiom of including PASS/FAIL or VALIDATE/ERROR in the Properties.

This lets you fold the logic into the one expression. Or you could write a logical expression here and keep the logic separate.

```Haskell
instance Proper MyModel where
  expect =  All $ Var <$> [ This
                          , That
                          , TheOther
                          ]
```

### How do we give meaning to the Properties?
Through the satisfiesProperty function we can define any meaning we like.

```Haskell
Instance Model MyModel where
  satisfiesProperty :: Model MyModel -> Property MyModel -> Bool
```

### Where do the property tests come from?
By providing a base generator for the Model we get a Property Set parameterised generator for free.

This default generator is composed from a sequence filters of the form `\model -> HedgeHog.Gen.filterT (satisfiesProperty model property) gen` and may be quite inefficient since it uses rejection sampling.

Fortunately we can reduce the use of the filter by widening it's grasp.

We just need to define transformations that can operate on Models then we can take a Model that is close to what we want and modify it to be exactly what we are looking for.

To do this we can define the transformations in terms of logical expressions.

```Haskell
class Proper model =
  data Transformation model m =
    Transformation {
      match :: Formula (Property model)
    , result :: Set (Property model)
    , genTransform :: Model model -> m (Model model)
    }
```

We could then say something like this.

```Haskell
instance Proper MyModel where
  transformations = :: MonadGen m => [Transformation MyModel m]
  transformations = [Transformation {
      match = (Var This :->: Var That) :&&: Not (Var WhateverThisIs)
    , result = All $ Var <$> [This,That,TheOther]
    , genTransform model = do
        if satisfiesProperty model This
          then do
            a <- genTheOtherThisWay
            return model { theOther = a}
          else do
            a <- genThis
            b <- genThat a
            c <- genTheOtherTheOtherWay a b
            return model { this = a, that = b, theOther = c}
    }
    ]
```

Now when we construct the filter for our generator we first check if we have a direct hit then check if we can find a path of transformations that can get us there. If either of these are true we accept the generated Model. Since the result field of a transformation is a property set we can compute the Transformation Graph at test suite start up by enumerating the set of allowed Transformations compositions. The algorithm for traversing the transformation graph is a random path algorithm to aid in finding Model consistency errors (the Transformations are really part of the model and the `match` and `result` expressions form a contract that we want genTransform to follow).

Now to generate a Model that satisfies a Property Set we can filter generated Models based on whether they are connected to the target Property Set via the transformation graph and then randomly traverse the graph toward the target Property Set.

#### Can I match on Yes?
Yes you can. Matching on Yes is like saying to take this edge in the transformation graph you don't need to bother doing generate and test at all. If the target Model is connected to a Transformation such as this then we bypass the default generator entirely. This means you can fully replace the inefficient default generator with custom Transformations such that you don't call `HedgeHog.Gen.filterT` at all.

### What's a model consistency error?
This is a very powerful feature of this approach to testing. We are making a complex model so wouldn't it be nice to be able to test that it makes sense?

It turns out that we can now do this very easily. Since our Model generator is parameterised by a Property Set and we can derive a Property Set from a Model via `satisfiesProperty` we can test that the logic encoded in our generator Transformations is consistent with the logic in `satisfiesProperty` by enumerating all allowed Property Sets and using them to generate corresponding Models which we then check satisfy the Properties in the set used to parameterise the generator. This closed loop tests the logic we encode in the generator matches the logic we encode in the properties.

This means that we can develop and test a model for the device we want to create before writing a single line of code for the device.

### How do we run a device test?
We need to define a translation of the Model into an encoding that can be input into the device test runner. This may require some wrapper code. There is a generic minimal version of the framework in `Proper.Minimal` and a UPLC CEK machine test runner that can run a compiled Plutus script in `Proper.Script`.

## What are the goals of this project?
To test code extremely thoroughly by generating test suites instead of constructing them by hand.

This has a number of advantages over the traditional hand written approach.
1. Exhaustiveness
We can be sure that we are exploring the entire space of properties we have defined. The definition of this space is the `logic` expression which we exhaustively enumerate solutions to with a SAT solver.
2. Separation of specification from implementation
By constructing and testing a model before writing the device code we can take a principled specification approach to developing complex software.
3. Perspective and visibility
The specification is encoded not just in the properties: it is co-representated by the Generator Transformations which must be consistent with Properties. This gives us three points of perspective from which we can view the device behaviour (Properties, Transformations, and the device implementation itself). This triangulation of perspective increases the chance of spotting logical errors in an audit or review - it is recommended that each component of the model and implementation logic be independently authored for this reason.


## How do I use this?
I highly recommend reading the examples and implementation. It's not too much code and it will really help in understanding the library. If you feel you need more documentation or description of any aspect of the library please submit an issue.

## What can I do with this?
You can test all the things in all the ways you can think of without having to write individual tests for each permutation by hand. If your model is complex enough I expect you will generate many more property tests than you write lines of code for the model. If you find you are generating too many tests then you can filter them using a logical expression. You can also use these logical expressions to subdivide your test suite into long (exhaustive) and short (subset) tests.

The code is licensed under Apache 2.0; check the LICENSE.md file for details.
