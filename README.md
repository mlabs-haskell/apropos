# `apropos`
Propositional logic apropos types.

Generate a suite of property tests from a logical specification.


  - Enrich your types with propositional logic over an associated property type.

  - Write generators parameterised by this logic.

  - Generate test suites that search the space of expected behaviours looking for counterexamples.


## What is this?
`apropos` specifications are composable stochastic search directives. The search is orchestrated by a SAT solver. This approach to testing can be viewed as Satisfiability Modulo Theories (SMT) where the theories are property tests run against code and satisfiability is whether the tested code behaves in the correct way for every test created by the model.

While we may not be able to run every property test that the model can generate we can achieve a probabilistic notion of satisfiability that allows us to say that it is very likely that the tested code exactly meets the specification.

### How to write a specification
Specifications are encoded as logical models associated with a type.

```Haskell
data IntProp
  = IsNegative
  | IsPositive
  | IsZero
  | IsLarge
  | IsSmall
  | IsMaxBound
  | IsMinBound
  deriving stock (Eq, Ord, Enum, Show, Bounded)
  deriving anyclass Enumerable
```
Here is a model for integers. We have partitioned the space of integers into a smaller easier to manage space. This will be a sufficient description to test the example function defined later. It is sufficient since the function should return true/false according to a formula defined in terms of the above propositions. By modelling integers like this we also make sure that we test some specific extremal values in the space.

Some of these propositions don't make sense in combination. For example an integer can't be both positive and negative. To express this we use a logical expression to define the space of valid property sets under this model.

```Haskell
instance LogicalModel IntProp where
  logic =
    ExactlyOne [Var IsNegative, Var IsPositive, Var IsZero]
      :&&: ExactlyOne [Var IsLarge, Var IsSmall]
      :&&: (Var IsZero :->: Var IsSmall)
      :&&: (Var IsMaxBound :->: (Var IsLarge :&&: Var IsPositive))
      :&&: (Var IsMinBound :->: (Var IsLarge :&&: Var IsNegative))
```

In order for this to be a model of a type we need to specify what each proposition means in terms of that type. We do this by associating the propositions and the type with the class `HasLogicalModel` which requires us to implement `satisfiesProperty`.

```Haskell
instance HasLogicalModel IntProp Int where
  satisfiesProperty IsNegative i = i < 0
  satisfiesProperty IsPositive i = i > 0
  satisfiesProperty IsMaxBound i = i == maxBound
  satisfiesProperty IsMinBound i = i == minBound
  satisfiesProperty IsZero i = i == 0
  satisfiesProperty IsLarge i = i > 10 || i < -10
  satisfiesProperty IsSmall i = i <= 10 && i >= -10
```

To generate our test suites we require a generator that is parameterised by the logical model for this enriched type. This will allow us to efficiently sample from random spaces defined by logical formulas. We can use this generator to exhaustively enumerate all scenarios encoded by the model as property tests. We can also use it compositionally in specifications that depend on this model. For example `genSatisfying (Var IsNegative :: Formula IntProp)` must generate negative integers for us.

This generator implements the class `HasParameterisedGenerator`.

```Haskell
instance HasParameterisedGenerator IntProp Int where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen Int
baseGen = int (linear minBound maxBound)
```

Here we are using a generator construction method that allows us to build such a generator.

This construction method gives us the function `buildGen` which we can compose with a base generator for integers to build a parameterised generator.


Each PermutationEdge must have a unique name. It can match on a subset of the property space with the match clause. The contract specifies how the morphism acts on a property set. The permuteGen is a random morphism on the type.

```Haskell
instance HasPermutationGenerator IntProp Int where
  generators =
    [ Morphism
        { name = "MakeZero"
        , match = Not $ Var IsZero
        , contract = clear >> addAll [IsZero, IsSmall]
        , morphism = \_ -> pure 0
        }
    , Morphism
        { name = "MakeMaxBound"
        , match = Not $ Var IsMaxBound
        , contract = clear >> addAll [IsMaxBound, IsLarge, IsPositive]
        , morphism = \_ -> pure maxBound
        }
    , Morphism
        { name = "MakeMinBound"
        , match = Not $ Var IsMinBound
        , contract = clear >> addAll [IsMinBound, IsLarge, IsNegative]
        , morphism = \_ -> pure minBound
        }
    , Morphism
        { name = "MakeLarge"
        , match = Not $ Var IsLarge
        , contract = clear >> addAll [IsLarge, IsPositive]
        , morphism = \_ -> int (linear 11 (maxBound -1))
        }
    , Morphism
        { name = "MakeSmall"
        , match = Not $ Var IsSmall
        , contract = clear >> addAll [IsSmall, IsPositive]
        , morphism = \_ -> int (linear 1 10)
        }
    , Morphism
        { name = "Negate"
        , match = Not $ Var IsZero
        , contract =
            branches
              [ has IsNegative >> remove IsNegative >> add IsPositive
              , has IsPositive >> remove IsPositive >> add IsNegative
              ]
        , morphism = \i -> pure (- i)
        }
    ]


```

The construction method involves building a directed graph of random morphisms on the type. The nodes of this graph are sets of properties. Since the functions state their type as a transformation on these sets we can check that the graph is reachable from every vertex. If we have built such a fully connected graph we can compute a transformation between any logical description of the type in terms of the associated propositions. This lets us lift a general non parameterised generator to a parameterised generator by a graph traversal. These traversals are found via a random shortest path algorithm.

### How to run tests

Given these generators we can now perform a model consistency test. We can define this test suite in terms of a logical formula over the propositions. This lets us for example only run tests that deal with negative integers by specifying `Var IsNegative` in place of `Yes :: Formula IntProp` which enumerates the whole space.

This test will check that every edge defined above behaves according to its contract. It will also provide helpful errors if the generators do not behave as specified.

```Haskell
intPermutationGenSelfTests :: TestTree
intPermutationGenSelfTests =
  testGroup "intPermutationGenSelfTests" $
    fromGroup
      <$> permutationGeneratorSelfTest
        True
        (\(_ :: Morphism IntProp Int) -> True)
        baseGen
```

We can now run the test suite against some pure function that returns `Bool`. It must return `Bool` in accordance with the definition of `expect` otherwise tests will fail.

```Haskell

instance HasPureRunner IntProp Int where
  expect _ = Var IsSmall :&&: Var IsNegative
  script _ i = i < 0 && i >= -10

intPermutationGenPureTests :: TestTree
intPermutationGenPureTests =
  testGroup "intPermutationGenPureTests" $
    fromGroup
      <$> [ runPureTestsWhere (Apropos :: Int :+ IntProp) "AcceptsSmallNegativeInts" Yes
          ]
```

For more information please see the `examples` directory.

The code is licensed under Apache 2.0; check the LICENSE.md file for details.
