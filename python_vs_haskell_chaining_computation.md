# Python vs Haskell

## basic syntax comparison

```python
def add_one(i: float) -> float:
  return i + 1
```


```haskell
addOne :: Num a => a -> a
addOne x = x + 1
```

## motivational comparison

```python
from __future__ import division

import math
from typing import Optional

def safe_divide(num: float, den: float) -> Optional[float]:
  if den == 0:
    return None
  else:
    return num / den

def silly_function(d: float) -> Optional[float]:
  quotient = safe_divide(36, d)
  if quotient is None:
    return None
  quotient = safe_divide(14, quotient + 10)
  if quotient is None:
    return None
  return quotient - 10

>>> silly_function(13)
-8.903614457831326
>>> silly_function(0)
None
```

```haskell
safeDivide :: RealFrac a => a -> a -> Maybe a
safeDivide num den = if den == 0 then Nothing else Just (num / den)

sillyFunction :: RealFrac a => a -> Maybe a
sillyFunction d = safeDivide 36 d >>=
                  \q -> Just (q + 10) >>=
                  safeDivide 14 >>=
                  \q -> Just (q - 10)

> sillyFunction 13
Just (-8.903614457831326)
> sillyFunction 0
Nothing
```

## reiterating the point

```Haskell
-- in haskell it's totally chill to do the following chaining
safeDivide 100 x >>= safeDivide 100 >>= safeDivide 100 >>= safeDivide 100 ...
```

```python
# but in python you have to explicitly do that error handling yourself
# which is error prone, especially when you don't have type checking
result = safe_divide(100, x)
if result is not None:
    result = safe_divide(100, result)
    if result is not None:
        result = safe_divide(100, result)
        if result is not None:
            result = safe_divide(100, result)
            ...
```
