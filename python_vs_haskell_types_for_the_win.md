# Python vs Haskell

Let's say you're building a finance application. One thing you want to be able to do is to combine money. Your first attempt in python might look something like this:

```python
CURRENCIES = [
  "USD",
  "YEN",
  "XI",
]

class Money(object):
    def __init__(self, amount: int, currency: str):
        if not isinstance(amount, int):  # we _could_ rely on type hints but better safe than sorry
            raise Exception("Amount must be integer")
        if currency not in CURRENCIES:
            raise Exception("Currency must be one of the following: {}".format(CURRENCIES))
        self.amount = amount
        self.currency = currency

def add_money(money1: Money, money2: Money) -> Money:
    currency = money1.currency
    if currency != money2.currency:
        raise Exception("Currency types must match")  # TODO write custom Exception
    new_amount = money1.amount + money2.amount
    return Money(new_amount, currency)
```

The above is basically fine and works like you'd expect:
```python
> m1 = Money(12, "YEN")
> m2 = Money(8, "USD")
> m3 = Money(22, "YEN")

> m4 = add_money(m1, m3)  # works fine
> print(m4.amount)
34
> m5 = add_money(m1, m2)  # blows up at runtime
Exception: Currency types must match
```

Ugh. Runtime errors suck. It's also annoying that we have to write the type validation code by hand. Furthermore, any function that calls `add_money` has to _know_ that it might raise an exception and explicitly handle it.

I bet we can do better in Haskell.
