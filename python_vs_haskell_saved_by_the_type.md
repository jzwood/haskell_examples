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
