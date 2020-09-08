# Python vs Haskell

Let's say you're going to make a new 2D vector math library. Your first take in python might implement some of the following functions.

```python
import math

from typing import Tuple


Vector = Tuple[float, float]
Point = Tuple[float, float]
Scalar = int

def to(p1: Point, p2: Point) -> Vector:
    """ returns Vector from p1 to p2 """
    px1, py1 = p1
    px2, py2 = p2
    return px2 - px1, py2 - py1

def mag(v: Vector) -> Scalar:
    """ returns the magnitude of the vector """
    vx, vy = v
    return math.sqrt(vx * vx + vy * vy)
```

This seems reasonable but b/c the structure of `Vector` and `Point` is the same, type hinting won't protect us from performing nonsensical things.

```python
vector1 = (0.0, 1.0)
vector2 = (2.0, 2.0)
point1 = (3.0, -1.0)

>>> to(vector1, vector2)
(2.0, 1.0)
mag(point1)
>>> mag(point1)
3.1622776601683795
```

What would this look like in Haskell?

```haskell
type Vector = (Float, Float)
type Point = (Float, Float)
type Scalar = Float

to :: Point -> Point -> Vector
to (px1, py1) (px2, py2) = (px2 - px1, py2 - py1)

mag :: Vector -> Scalar
mag (vx, vy) = sqrt $ vx^2 + vy^2
```

Super compact but we can still do silly things:
```haskell
vector1 = (0.0, 1.0)
vector2 = (2.0, 2.0)
point1 = (3.0, -1.0)

> to vector1 vector2
(2.0, 1.0)
> mag point1
3.1622776601683795
```

This is one of the big reasons to use `newtype`!

```haskell
newtype Vector = Vector (Float, Float) deriving (Show)
newtype Point = Point (Float, Float) deriving (Show)
newtype Scalar = Scalar Float deriving (Show)

to :: Point -> Point -> Vector
to (Point (px1, py1)) (Point (px2, py2)) = Vector (px2 - px1, py2 - py1)

mag :: Vector -> Scalar
mag (Vector (vx, vy)) = Scalar (sqrt $ vx^2 + vy^2)

point1 = Point (3.0, 2.0)
point2 = Point (3.0, 4.0)

> mag point1
Couldn't match expected type ‘Vector’ with actual type ‘Point’
> to point1 point2
Vector (0.0,2.0)
```

Very cool and very safe. There are a lot of neat things you can do with newtype, for example treating newtype like a num.

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype USD = USD Int deriving (Show, Eq, Ord, Num)
newtype Yen = Yen Int deriving (Show, Eq, Ord, Num)

> USD 3 + USD 2
USD 5
> USD 3 > USD 6
True
> USD 3 + Yen 10  -- caught at compile time
  Couldn't match expected type ‘USD’ with actual type ‘Yen’
    In the second argument of ‘(+)’, namely ‘Yen 10’
    In the expression: USD 3 + Yen 1
```

What is the deriving doing? It's automating the following instance implementations:

```haskell
instance Eq USD where
  (==) (USD a) (USD b) = a == b

instance Ord USD where
  (>) (USD a) (USD b) = a > b

instance Num USD where
  (+) (USD a) (USD b) = USD (a + b)
  (*) (USD a) (USD b) = USD (a * b)
  ..
```

It might feel weird to have such specific custom types. It _seems_ like it should make your code less reusable. Like say you've got a super sick new type:

```haskell
import Data.Char

newtype CoolLetter = CoolLetter Char deriving (Show)
```

and you also want to use a library that acts on a subset of that type, aka `Char`.

```haskell
-- some library function
isCool :: Char -> Bool
isCool = flip elem "JAKE"
```

To use `isCool` we now need to write a custom adapter:

```haskell
-- naive one-off adapter function
isCool' :: CoolLetter -> Bool
isCool' (CoolLetter c) = isCool c
```

Not too hard but gonna get annoying fast if we have to do that for every 3rd party function. A better idea would be to implement a generic adapter:

```haskell
-- smart generic adapter function
apply :: (Char -> a) -> CoolLetter -> a
apply f (CoolLetter c) = f c
```

Now we can stufff `CoolLetter` into any `Char -> *` kinded function.

```haskell
> apply isCool (CoolLetter 'N')
False
```

What if we had a more complicated datatype and we wanted to be able to apply functions to internal structure?

```haskell
data LabeledType a = LabeledType a String deriving (Show)

instance Functor LabeledType where
  fmap f (LabeledType a s) = LabeledType (f a) s
```

Now we can take any function that consumes a `Char` and we can apply that to the internals of `LabeledType`.

```haskell
> myLetter = LabeledType 'j' "a very cool letter"
> fmap toUpper myLetter
LabeledType 'J' "a very cool letter"
```

The takeaway here is that it's really easy in Haskell to interoperate generic functions with highly specific datatypes.
