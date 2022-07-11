```haskell
Prelude> import Control.Applicative
Prelude> Just (+1) <*> Just 3
Just 4
Prelude> :t (+) <$> Just 3
(+) <$> Just 3 :: Num a => Maybe (a -> a)
Prelude> (+) <$> Just 3 <*> Just 1
Just 4
Prelude Control.Applicative> :set -XTupleSections
Prelude Control.Applicative> liftA2 (+) (Just 1) (Just 2)
Just 3
Prelude Control.Applicative> (+1) <*> (+1) (+1)
Prelude Control.Applicative> :t (+) <*> (+1)
(+) <*> (+1) :: Num a => a -> a
Prelude Control.Applicative> :t (+) <$> (+1)
(+) <$> (+1) :: Num a => a -> a -> a
Prelude Control.Applicative> (,) <$> (*2) <*> (+2) $ 9
(18,11)
Prelude Control.Applicative> :t fmap (,) (*2)
fmap (,) (*2) :: Num a => a -> b -> (a, b)
Prelude Control.Applicative> (,) <$> (*2) <*> (+2) $ 9
(18,11)
Prelude Control.Applicative> (,) . (*2) <*> (+2) $ 9
(18,11)
Prelude Control.Applicative> :set -XTupleSections
Prelude Control.Applicative> \x -> (2*x,) <*> (+2) $ 9
(18,11)
Prelude Control.Applicative> (\x -> (,) (2*x)) <*> (+2) $ 9
(18,11)
Prelude Control.Applicative> (3,) 4
(3,4)
Prelude Control.Applicative>
Prelude Control.Applicative> (\x -> (2*x,)) <*> (+2) $ 9
(18,11)
Prelude Control.Applicative>
Prelude Control.Applicative> (+1) <$> Just 3
Just 4
Prelude Control.Applicative> (+1) <$> [4]
[5]
Prelude Control.Applicative> Just (+1) <$> Just 3  -- won't work
<interactive>:37:1: error:
    • Couldn't match expected type ‘a1 -> b’
                  with actual type ‘Maybe (a0 -> a0)’
Prelude Control.Applicative> Just (+1) <*> Just 3
Just 4
Prelude Control.Applicative> Just (+1) <*> Nothing
Nothing
Prelude Control.Applicative> Nothing <*> Just 3
Nothing
Prelude Control.Applicative> Just (+1) <*> Just 3
Just 4
Prelude Control.Applicative> [(+1)] <*> [4]
[5]
Prelude Control.Applicative> [(+1)] <*> [4, 4]
[5,5]
Prelude Control.Applicative> (+1) <$> (+1) $ 6
8
Prelude Control.Applicative> (+1) <$> (*1) $ 6
7
Prelude Control.Applicative> (+1) <$> (*2) $ 6
13
Prelude Control.Applicative> (,) 3 4
(3,4)
Prelude Control.Applicative> (,) <$> (*2) <*> (+2) $ 9
(18,11)
Prelude Control.Applicative> (,) . (*2) <*> (+2) $ 9
(18,11)
Prelude Control.Applicative> (\x -> (x*2,)) <*> (+2)$  9
(18,11)
Prelude Control.Applicative> ((\x -> (x*2,)) <*> (+2)) 9
(18,11)
Prelude Control.Applicative> ((\x -> (x*2,)) <*> (+2)) 9
(18,11)
Prelude Control.Applicative> (\x -> (x*2,)) <*> (+2) $ 9
(18,11)
Prelude Control.Applicative> (\x -> (x*2,x+2)) $ 9
(18,11)
Prelude Control.Applicative> (\x -> (x*2,x+2)) 9
(18,11)
Prelude Control.Applicative> (+) <$> (*2) <*> (+2) $ 9
29
Prelude Control.Applicative> data User = User String Integer String deriving (Show, Eq)
Prelude Control.Applicative> User "bob" 30 "green"
User "bob" 30 "green"
Prelude Control.Applicative> name = Just "bob"
Prelude Control.Applicative> age = Just 30
Prelude Control.Applicative> :t name
name :: Maybe [Char]
Prelude Control.Applicative> color = Just "green"
Prelude Control.Applicative> :t User
User :: String -> Integer -> String -> User
Prelude Control.Applicative> User <$> name <*> age <*> color
Just (User "bob" 30 "green")
Prelude Control.Applicative> :t fmap User name
fmap User name :: Maybe (Integer -> String -> User)
Prelude Control.Applicative> User <$> Nothing <*> age <*> color
Nothing
Prelude Control.Applicative> (\n a c -> n ++ c) <$> name <*> age <*> color
Just "bobgreen"
Prelude Control.Applicative> User <$> name <*> age <*> color
Prelude Control.Applicative> User <$> name <*> age <*> color
Just (User "bob" 30 "green")
Prelude Control.Applicative> :t name
name :: Maybe [Char]
Prelude Control.Applicative> name = \m -> m ++ "bob"
Prelude Control.Applicative> name "mr."
Prelude Control.Applicative> age = \p -> 40
Prelude Control.Applicative> color = \m -> m ++ "green"
Prelude Control.Applicative> User (name "mr") (age "") (color "super")
User "mrbob" 40 "supergreen"
Prelude Control.Applicative> :t User <$> name <*> age <*> color
User <$> name <*> age <*> color :: [Char] -> User
Prelude Control.Applicative> User <$> name <*> age <*> color $ "cool-"
User "cool-bob" 40 "cool-green"
```
