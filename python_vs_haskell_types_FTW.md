# Python vs Haskell

Let's say you're building a finance application. One thing you want to be able to do is to sum like currency. Your first attempt in python might look something like this:

```python
from enum import Enum, auto

class Currency(Enum):
    USD = auto()
    YEN = auto()
    XI = auto()

class Money(object):
    def __init__(self, amount: int, currency: Currency):
        if not isinstance(amount, int):  # we _could_ rely on type hints but better safe than sorry
            raise TypeError("amount must be integer")
        if not isinstance(currency, Currency):
            raise TypeError("currency must member of Currency class")
        self.amount = amount
        self.currency = currency

    def add(self, money: Money):
        currency = money.currency
        if currency != self.currency:
            raise ValueError("Currency types must match")
        self.amount += money.amount
```

The above is basically fine and works like you'd expect:
```python
> m1 = Money(12, Currency.YEN)
> m2 = Money(8, Currency.USD)
> m3 = Money(22, Currency.YEN)

> m1.add(m3)  # works fine
> print(m1.amount)
34
> m1.add(m2)  # blows up at runtime
Exception: Currency types must match
```

Ugh. It's also annoying that we have to write the type validation code by hand. Furthermore, any function that calls `add` has to _know_ that it might raise an exception and explicitly handle it.

I bet we can do better in Haskell.

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype Yen = Yen Int deriving (Show, Eq, Ord, Num)
newtype USD = USD Int deriving (Show, Eq, Ord, Num)
newtype Xi = Xi Int deriving (Show, Eq, Ord, Num)
```

Done! We've actually accomplished a LOT more than the python version. Yen, USD, and Xi are effectively type-safe integers.

```haskell
> Yen 3 + Yen 2
Yen 5
> Xi 3 * Xi 4
Xi 12
> USD 10 + Yen 10  -- caught at compile time
Couldn't match expected type ‘USD’ with actual type ‘Yen’
In the second argument of ‘(+)’, namely ‘Yen 10’
  In the expression: USD 10 + Yen 10
  In an equation for ‘it’: it = USD 10 + Yen 10
```
