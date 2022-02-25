# `apropos`
Propositional logic apropos arbitrary types.

Generate a suite of property tests from a logical specification of program behaviour.

Types are specifications for the properties of a program but they can't encapsulate everything.
  - Enrich your types with propositional logic over an associated property type.
  - Write generators parameterised by this logic.
  - Generate test suites that search the space of expected behaviours looking for counterexamples.

`apropos` gives you a tool set for employing this specification strategy in a hierarchichal way. Specifications can be composed to build a detailed specification of a complex program.

## What is this?
It's a thing that generates property tests for you from a specification.

### What kind of specification?
Specifications are encoded as logical models associated with a type.

### And how do you write this logical model?
Good question. First you start with a type. Then you describe its properties.

```Haskell
data MyModel =
  This | That | TheOther { things :: [Thing] }
```

```Haskell
data MyProperty =
    DoesThis
  | DoesThat
  | DoesTheNextThing
  deriving (Bounded,Eq,Ord,Enum,Show)

instance Enumerable MyProperty where
  enumerated = [minBound..maxBound]
```

(The Enumerable class and some template haskell makes it easy to embed these Properties into a containing model. More on that later.)


We can now write Formulas in propositional logic over the property type.


For example:

```Haskell
term = (Var DoesThis :->: Var DoesThat)
    :&&: (Var DoesThat :->: Var DoesTheNextThing)

```

Terms like these are used to build our specification.

#### Model Internal Logic
This is a term you define to constrain which properties of the model can be true at the same time.

```Haskell
instance LogicalModel MyProperty where
  logic = Var This :->: Var That
```

For example if This implies That then whenever This is true That must also be true.

#### When we expect the device under test to PASS or FAIL
This tells the test runner when to expect Passing/Failing of the device.

```Haskell
instance HasPureRunner MyModel MyProperty where
  expect = Var VALIDATES

```

You could use the idiom of including PASS/FAIL or VALIDATE/ERROR in the Properties.

Or you could write a logical expression here and keep the behaviour specification separate.

```Haskell
instance HasPlutusTestRunner Model MyProperty where
  expect =  All $ Var <$> [ This
                          , That
                          , TheOther
                          ]
```

### How do we give meaning to the Properties?
Through the satisfiesProperty function we can define any meaning we like.

```Haskell
instance HasLogicalModel MyModel MyProperty where
  satisfiesProperty :: Model MyModel -> Property MyModel -> Bool
```

### Where do the property tests come from?
We have to construct a random generator that is parameterised by a set of properties.

```Haskell
instance HasParameterisedGenerator MyModel MyProperty where
  parameterisedGenerator :: Set MyProperty -> Gen MyModel
```

This lets us enumerate the space of property sets and create a property test for each set.

### We can write this in a different way
By defining a collection of random generators that can transform a model satisfying some properties to a model satisfying a different set of properties.

These generators must obey a contract such that the effect they have on the properties of a model is deterministic. If this is true we can precompute a graph of edges between models. If this graph is strongly connected then we can get to any model from any model along a path that connects them in the graph.

These edges are like dependently typed (with respect to properties) random morphisms on the type we are modelling.

```Haskell
data PermutationEdge p m =
  PermutationEdge {
    name :: String
  , match :: Formula p
  , contract :: Contract p ()
  , permuteGen :: m -> Gen m
  }
```

These edges have a name, match on a set of properties with a Formula, obey a property set transformation contract, and perform a random permutation of the model.


If you can provide a set of these that form a strongly connected graph then you'll have something you can plug into HasParameterisedGenerator. All the graph building, path traversal, and contract checking is done for you. The selfTest generated test suite will give you compiler like feedback on errors in the graph.

```Haskell
class (HasLogicalModel p m, Show m) => HasPermutationGenerator p m where
  generators :: [PermutationEdge p m]
  buildGen :: forall t . Monad t => Gen m -> Set p -> PropertyT t m
  buildGen g = ... -- you get the rest for free!
```

See `examples/Spec/IntPermutationGen.hs` for a simple example.

### What's a model consistency error?
This is a very powerful feature of this approach to testing. When making a complex model so wouldn't it be nice to be able to test that it makes sense?

Since our generator is parameterised by a set of properties this allows us to test that it returns a model that satisfies this set of properties. This consistency test checks that the logic encoded by the generator matches the logic encoded by the properties.

If we are using the PermutingGenerator to build our parameterised generator each edge must obey its contract. This provides additional testing of our assumptions about the model. The contracts are checked whenever an edge is traversed during generation.

### How do we hook up some code to test?

We need to define a translation of the Model into an encoding that can be input into the device test runner. This may require some wrapper code. To test an arbitrary pure function you can use `Apropos.Pure`.

```Haskell
instance HasPureRunner MyProperty MyModel where
  expect _ = Var This :||: Var That
  script _ m = myScript m
```

## Read the examples.
For a minimal example see `examples/Spec/Int.hs`

For the same example but using the PermutingGenerator see `examples/Spec/IntPermutingGen.hs`

## What are the goals of this project?
To test code extremely thoroughly by generating test suites instead of constructing them by hand.

This has a number of advantages over the traditional hand written approach.
1. Exhaustiveness
We can be sure that we are exploring the entire space of properties we have defined. The definition of this space is the `logic` expression which we exhaustively enumerate solutions to with a SAT solver.
2. Compositionality
We can compose models and hide details of the submodels. This is easy to do with `genSatisfying (someExpression :: Formula SomeProperties)`
3. Separation of specification from implementation
By constructing and testing a model before writing the device code we can take a principled specification approach to developing complex software.
4. Perspective and visibility
The specification is encoded not just in the properties: it is co-representated by the Generator Transformations which must be consistent with Properties. This gives us three points of perspective from which we can view the device behaviour (Properties, Transformations, and the device implementation itself). This triangulation of perspective increases the chance of spotting logical errors in an audit or review - it is recommended that each component of the model and implementation logic be independently authored for this reason.


## How do I use this?
Start by looking at the examples.

## What can I do with this?
You can test all the things in all the ways you can think of without having to write individual tests for each permutation by hand. If your model is complex enough I expect you will generate many more property tests than you write lines of code for the model. If you find you are generating too many tests then you can filter them using a logical expression. You can also use these logical expressions to subdivide your test suite into long (exhaustive) and short (subset) tests.

The code is licensed under Apache 2.0; check the LICENSE.md file for details.
