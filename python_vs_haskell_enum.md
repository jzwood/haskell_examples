# Python vs Haskell

### Enums

Enums in python where we don't care about the specific value, just membership in enum group.

```python
from enum import Enum, auto

class Color(Enum):
    RED = auto()
    GREEN = auto()
    BLUE = auto()

def how_much_do_i_like_color(color: Color) -> int:
    if color == Color.RED:
        return 4
    if color == Color.BLUE:
        return 7
    if color == Color.GREEN:
        return 8
    return 0

>>> how_much_do_i_like_color(Color.GREEN):
8
```

There are different techniques for achieving enum like structures in Haskell but the most idiomatic haskell (IMHO) is to just use the type system.

```haskell
data Color = RED | GREEN | BLUE deriving (Show, Eq, Ord)

howMuchDoILikeColor :: Color -> Int
howMuchDoILikeColor c =
  case c of
    RED -> 4
    BLUE -> 7
    GREEN -> 8
    _ -> 0

> howMuchDoILikeColor GREEN
8
```
