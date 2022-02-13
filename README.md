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
We have to construct a random generator that is parameterised by a set of properties.

```Haskell
instance Proper MyModel where
  genBaseModel :: MonadGen m => Set (Property MyModel) -> m (Model MyModel)
```

This lets us enumerate the space of property sets and create a property test for each set.

#### Why genBaseModel and not genModel?
This generator is not required to be perfect. It can generate models that don't satisfy the provided property set exactly so long as we construct suitable transformations to fix up these deficiencies. We could start with a genBaseModel that ignores the property set argument and construct our parameterised generetor entirely using random transformations.

#### What's a transformation?
A transformation is a type that we have to define in a similar way to Property.

```Haskell
instance Proper MyModel where
  data Transformation model =
    NullTransformation
    deriving stock (Eq,Ord,Enum,Bounded,Show)
```

If we want to start with no transformations in place we could write something like this.

These transformations much like properties have some associated functions that give them meaning.

```Haskell
instance Proper MyModel where
  modelTransformation :: MonadGen m => Transformation model -> Model model -> m (Model model)
  modelTransformation NullTransformation m = pure m
```

The effect a transformation has on a model is defined like this. It is a random function from a model to another model.

```Haskell
instance Proper MyModel where
  propertyTransformation :: Transformation model -> (Formula (Property model), Set (Property model) -> Set (Property model))
  propertyTransformation NullTransformation = (No, id)
```

A transformation must obey a contract defined by `propertyTransformation`. This contract is defined in terms of a Propositional Logic expression (Formula) which defines which models the transformation can be performed on AND a function that transforms a set of properties. This contract is checked by the test runner - if a transformation is applied and the transformed model's properties do not conform to the contract this will be reported as a failure.

Transformation paths are enumerated by search on the base generated model and contract specifications. We randomly choose from the set of possible transformation paths.

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
