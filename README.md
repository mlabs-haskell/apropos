# 'apropos'

Hedgehog generation that sniffs out edge cases.

## Why `apropos`?

Traditionally, Haskell programmers have used property testing, starting with the 
legendary [QuickCheck](https://hackage.haskell.org/package/QuickCheck), to test 
their code. This entails writing properties that should hold about your program,
essentially as functions from arbitrary types to `Bool`, and the library tests
that against random data, trying to find counterexamples that would represent
incorrect code.

The problem with this is, property testing uses relatively simple data 
distributions, and there are many bugs that are activated only by very specific 
values - so-called 'edge cases', that these generators are very unlikely to find.

We can motivate this with an example, based on <https://github.com/nick8325/quickcheck/issues/98>:

The [abs](https://hackage.haskell.org/package/base/docs/Prelude.html#v:abs)
function from Prelude is supposed to return the absolute value of a number. 

Therefore, this function should always hold:

```haskell
absIsAlwaysPositive :: Int -> Bool
absIsAlwaysPositive n = abs n >= 0
```

Unfortunately, due to two's complement arithmetic, the negation of
`minBound :: Int` is not representable as an `Int`. Prelude's `abs` function just
 gives up and returns `minBound`. Hence:

 ```haskell
 >>> abs minBound :: Int
-9223372036854775808
>>> absIsAlwaysPositive minBound
False
```

Property testing is unlikely to catch this, though:
```haskell
>>> quickCheck absIsAlwaysPositive
+++ OK, passed 100 tests.
```

The solution is to write additional unit or property tests for any edge cases you
can think of. This quickly becomes tedious. As the complexity of your code grows,
this can become more and more of a problem, and you won't know if you've
missed something.

## What is `apropos`?
`apropos` integrates with the [Hedgehog]() testing library and attempts to solve this problem using 'description types', which describe and automatically test against
edge cases.

The core of `apropos` is the `Description` typeclass. You define an instance of
this class to start testing your code.

```haskell
class Description d a | d -> a where
  describe :: a -> d

  refineDescription :: Formula (Attribute d)
  refineDescription = Yes

  genDescribed :: (MonadGen m) => d -> m a
```

`a` is the type of your test data. At present, only one value can be tested
against at a time, but you can work around this by using a product type.

`d` is a type you define describing the interesting properties of `a`. This is 
known as the 'description type'.

You begin by defining a function `describe` that generates a description from a
value.

Whilst it is theoretically possible to define an ADT that precisely captures any
set of properties, in practice this may be difficult or unergonomic to do. Instead,
You may restrict which descriptions are valid using the optional `refineDescription`
method, using a logical formula. See the Haddocks for `Attribute` for more details.

Finally, you write a `Hedgehog` generator that generates a value of type `a` matching
a given description.

This allows `apropos`, using the magic of SAT solvers, to run a given test over
all combinations of properties, hopefully testing against all relevant edge cases.

Let's use this to find our bug in `abs`.

First, let's capture the interesting properties and possible edge cases of `Int`:
```haskell
data IntDescr = IntDescr
  { sign :: Sign
  , size :: Size
  , isBound :: Bool -- Is this equal to `minBound` or `maxBound`?
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (SOPGeneric, HasDatatypeInfo)
  
data Sign = Positive | Negative | Zero
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (SOPGeneric, HasDatatypeInfo)

data Size = Large | Small
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (SOPGeneric, HasDatatypeInfo)
```

We need `Ord` for the implementation of `apropos`. `Show` is needed by `Hedgehog`.
`Generic` (from `GHC.Generics`), `SOPGeneric` (a re-export from `generics-sop`), and `HasDatatypeInfo` (also from `generics-sop`) is boilerplate that is unfortunately required
for all description types, including the types of their fields recursively.

Now let's define our `Description` instance, asserting that `IntDescr` describes
`Int`:

```haskell
instance Description IntDescr Int where
```

Next, we derive desciptions:

```haskell
  describe :: Int -> IntDescr
  describe i =
    IntDescr
      { sign =
          case compare i 0 of
            GT -> Positive
            EQ -> Zero
            LT -> Negative
      , size =
          if i > 10 || i < -10
            then Large
            else Small
      , isBound = i == minBound || i == maxBound
      }
```

Not all constructible `IntDescr` values are valid descriptions - You can't have
a `Large` `Zero` or a `Small` `isBound`. So we implement `refineDescription` to
exclude these:

```haskell
  refineDescription :: Formula (Attribute IntDescr)
  refineDescription =
    All
      [ attr [("IntDescr", "sign")] "Zero" :->: attr [("IntDescr", "size")] "Small"
      , attr [("IntDescr", "isBound")] "True" :->: attr [("IntDescr", "size")] "Large"
      ]
```

We now write a Hedgehog generator that generates an `Int` matching a given
description:

```haskell
  genDescribed :: (MonadGen m) => IntDescr -> m Int
  genDescribed s =
    case sign s of
      Zero -> pure 0
      s' -> intGen s'
    where
      bound :: Sign -> Int
      bound Positive = maxBound
      bound Negative = minBound
      bound Zero = 0

      sig :: Sign -> Int -> Int
      sig Negative = negate
      sig _ = id

      intGen :: (MonadGen m) => Sign -> m Int
      intGen s' =
        if isBound s
          then pure (bound s')
          else case size s of
            Small -> int (linear (sig s' 1) (sig s' 10))
            Large -> int (linear (sig s' 11) (bound s' - sig s' 1))
```

The `Description` typeclass is lawful:

```haskell
-- Given a value `a` generated from a generator for a description `d` 
-- (genDescribed d), the description of `a` (describe a) equals `d`.
forall d. forAll (genDescribed d) >>= (\a -> describe a === d)
```

A `selfTest` method is provided to test that this law holds, and build confidence
that our `Description` instance is correct.

```haskell
intSimpleSelfTest :: Group
intSimpleSelfTest =
  Group
    "self test"
    (selfTest @IntDescr)
```

```haskell
self test
  IntDescr {sign = Positive, size = Large, isBound = False}: OK (0.03s)
      ✓ IntDescr {sign = Positive, size = Large, isBound = False} passed 100 tests.
  IntDescr {sign = Positive, size = Large, isBound = True}:  OK (0.03s)
      ✓ IntDescr {sign = Positive, size = Large, isBound = True} passed 100 tests.
  IntDescr {sign = Positive, size = Small, isBound = False}: OK (0.04s)
      ✓ IntDescr {sign = Positive, size = Small, isBound = False} passed 100 tests.
  IntDescr {sign = Negative, size = Large, isBound = False}: OK (0.05s)
      ✓ IntDescr {sign = Negative, size = Large, isBound = False} passed 100 tests.
  IntDescr {sign = Negative, size = Large, isBound = True}:  OK (0.03s)
      ✓ IntDescr {sign = Negative, size = Large, isBound = True} passed 100 tests.
  IntDescr {sign = Negative, size = Small, isBound = False}: OK (0.03s)
      ✓ IntDescr {sign = Negative, size = Small, isBound = False} passed 100 tests.
  IntDescr {sign = Zero, size = Small, isBound = False}:     OK (0.03s)
      ✓ IntDescr {sign = Zero, size = Small, isBound = False} passed 100 tests.
```

Now let's test our function!

We use the `runTests` function and the `AproposTest` type to define our test:

```haskell
runTests :: AproposTest d a -> [(s, Property)]

data AproposTest d a = AproposTest
  { expect :: d -> Bool
  , aproposTest :: a -> PropertyT IO ()
  }
```

`expect` is a predicate that defines whether the given description should cause 
the test to pass or fail. `apropos` by default also tests the negation of each 
property to ensure it fails. You can filter out properties you don't want to
test at all using `runTestsWhere`.

`aproposTest` is a `Hedgehog` property test that tests agains the given value `a`.

The return type of `runTests` is `IsString s => [(s, Property)]`, which can be 
plugged straight into Hedgehog's `Group`.

So our `abs` test looks like this:

```haskell
absTest :: Group
absTest =
  Group
    "apropos testing"
    $ runTests @IntDescr
      AproposTest
        { expect = const True -- should hold for all negative integers
        , aproposTest = \n -> assert $ abs n >= 0
        }
```

And we find the bug!

```haskell
    apropos testing
      IntDescr {sign = Positive, size = Large, isBound = False}: OK (0.04s)
          ✓ IntDescr {sign = Positive, size = Large, isBound = False} passed 100 tests.
      IntDescr {sign = Positive, size = Large, isBound = True}:  OK (0.02s)
          ✓ IntDescr {sign = Positive, size = Large, isBound = True} passed 100 tests.
      IntDescr {sign = Positive, size = Small, isBound = False}: OK (0.04s)
          ✓ IntDescr {sign = Positive, size = Small, isBound = False} passed 100 tests.
      IntDescr {sign = Negative, size = Large, isBound = False}: OK (0.04s)
          ✓ IntDescr {sign = Negative, size = Large, isBound = False} passed 100 tests.
      IntDescr {sign = Negative, size = Large, isBound = True}:  FAIL (0.03s)
          ✗ IntDescr {sign = Negative, size = Large, isBound = True} failed at src/Apropos/Runner.hs:25:9
            after 1 test.
          
               ┏━━ src/Apropos/Generator.hs ━━━
            15 ┃ runTest :: (Show a, Description d a) => (a -> PropertyT IO ()) -> d -> Property
            16 ┃ runTest cond d = property $ forAll (genDescribed d) >>= cond
               ┃ │ -9223372036854775808
            
               ┏━━ src/Apropos/Runner.hs ━━━
            20 ┃ runAproposTest :: forall d a. (Description d a, Show a) => AproposTest d a -> d -> Property
            21 ┃ runAproposTest atest d =
            22 ┃   runTest
            23 ┃     ( \a -> do
            24 ┃         b <- passes (aproposTest atest a)
            25 ┃         expect atest d === b
               ┃         ^^^^^^^^^^^^^^^^^^^^
               ┃         │ ━━━ Failed (- lhs) (+ rhs) ━━━
               ┃         │ - True
               ┃         │ + False
            26 ┃     )
            27 ┃     d
            28 ┃   where
            29 ┃     passes :: PropertyT IO () -> PropertyT IO Bool
            30 ┃     passes =
            31 ┃       PropertyT
            32 ┃         . TestT
            33 ┃         . ExceptT
            34 ┃         . fmap (Right . isRight)
            35 ┃         . runExceptT
            36 ┃         . unTest
            37 ┃         . unPropertyT
          
            This failure can be reproduced by running:
            > recheck (Size 0) (Seed 17184365450024726384 16839839501823744203) IntDescr {sign = Negative, size = Large, isBound = True}
          
        Use '--hedgehog-replay "Size 0 Seed 17184365450024726384 16839839501823744203"' to reproduce.
        
        Use -p '/apropos testing.IntDescr {sign = Negative, size = Large, isBound = True}/' to rerun this test only.
      IntDescr {sign = Negative, size = Small, isBound = False}: OK (0.06s)
          ✓ IntDescr {sign = Negative, size = Small, isBound = False} passed 100 tests.
      IntDescr {sign = Zero, size = Small, isBound = False}:     OK (0.01s)
          ✓ IntDescr {sign = Zero, size = Small, isBound = False} passed 100 tests.
```

Given `minBound` (`IntDescr {sign = Negative, size = Large, isBound = True}`), 
the test suite shows that the function return is incorrect.

Happy testing!