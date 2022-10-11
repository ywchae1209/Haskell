# Fuctor

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
    (<$>) :: (a -> b) -> f a -> f b
    f <$> fa = fmap f fa
```

## 1

```haskell
-- Functor IO from Monad IO
instance Functor IO where
    fmap f io = do
        value <- io
        return ( f value )
```

```haskell
import Data.Char
import Data.List

-- getLine :: IO String
fmap ( intersperse '-' . reverse . map toUpper ) getLine

```

## 2.

```haskell

-- function x -> a as Functor
instance Functor ((->) r ) where
    fmap f g = f . g
    -- g :: r -> a
    -- f :: a -> b
    -- fmap f g :: r -> b
```

## 3.

```haskell
(++) <$> Just "Spring" <*> Just "Summer"    -- Just "SpringSummer"
```

# Applicative Functor

```haskell
class (Functor f) => Appicative f where
    pure :: a -> f a
    (<*>) :: f ( a -> b) -> f a -> f b      -- apply function
    (<$>) :: (a -> b) -> f a -> f b


```

## 1

```haskell
instance Applicative Maybe where
    pure a = Just a
    (<*>) f Nothing = Nothing
    (<*>) f (Just a) = Just ( f a)
```

```haskell
    Just (+3) <*> Just 3
    Pure (+) <*> Just 2 <*> Just 5
```

## 2

```haskell
instance Applicative [] where
    pure a = [a]
    fs <*> xs = [ f a | f <- fs, x <- xs]
```

```haskell
[(+2),(-2)] <*> [1,2,3]    -- [3,4,5,-1,0,1]
[(+),(*)] <*> [1,2] <*> [3,4] -- [4,5,5,6,3,4,6,8]
```

```haskell
ghci> (++) <$> ["ha","heh","hmm"] <*> ["?","!","."]
["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]
```

```haskell
filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]  -- [55,80,100,110]

```

## 3

```haskell
instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return (f x)

```

```haskell
(++) <$> getLine <*> getLine
```

## 4 joining resource-consumer-function

```haskell
instance Applicative ((->) r ) where        -- r -> a
    pure x = (\_ -> x)
    f <*> g = (\x -> f x (g x))             -- g :: r -> a, f :: r -> (a -> b)
    f <$> g = f . g                         -- g :: r -> a, f :: a -> b

```

```haskell
ghci> (+) <$> (+3) <*> (*100) $ 5
-- add (+3)'s result and (*100)'s result
508

-- https://stackoverflow.com/questions/31152042/haskell-evaluation-of-3-100-5
-- in detail
(((+) <$> (+3))         <*> (*100)) $ 5        -- Add parens
((fmap (+) (+3))        <*> (*100)) $ 5        -- Prefix fmap
(((+) . (+3))           <*> (*100)) $ 5        -- fmap = (.)
((\a -> (+) ((+3) a))   <*> (*100)) $ 5        -- Definition of (.)
((\a -> (+) (a+3))      <*> (*100)) $ 5        -- Infix +
((\a b -> (+) (a+3) b)) <*> (*100)) $ 5        -- Eta expand
(\x -> (\a b -> (+) (a+3) b) x ((*100) x)) $ 5 -- Definition of (<*>)
(\x -> (\a b -> (+) (a+3) b) x (x*100)) $ 5    -- Infix *
(\a b -> (+) (a + 3) b) 5 (5*100)              -- Beta reduce
(\a b -> (a + 3) + b)   5 (5*100)              -- Infix +
(5 + 3) + (5*100)                              -- Beta reduce (twice)
508                                            -- Definitions of + and *

ghci> (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5

-- [ , , ]
--          . (+3)
--                 . (*2)
--                         . (/2)
--              5       5       5

[8.0,10.0,2.5]

```

## 5 ZipList, newtype

```haskell

-- data ZipList a = ZipList { getZipList :: [a] }
newtype ZipList a = ZipList { getZipList :: [a] }
-- newtype :: one constructor with one field.
-- when we want to just take one type and wrap it in something to present it as another type.
-- 'newtype' is faster than 'data' keyword


instance Applicative ZipList where
    pure x = ZipList (repeat x)     -- infinite List
    ZipList fs <*> ZipList xs = ZipList ( zipwith (\f x -> f x) fs xs)  -- apply f, x by position-wide
```

```haskell
(,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"
[('d','c','r'),('o','a','a'),('g','t','t')]
```

## lift, sequence

```haskell
Control.Applicative
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

liftA2 (:) (Just 3) ( Just 4) -- Just [3,4]
```

```haskell
sequenceA :: Applicative f => [f a] -> f [a]

sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs   -- recursive

-- other way
sequenceA = foldr ( liftA2 (:)) (pure [])
```

```haskell
sequenceA [ getLine, getLine, getLine]
heyh
ho
woo
["heyh","ho","woo"]
```

# Monoid

```haskell

import Data.Monoid

class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty

```

## 1

```haskell
instance Monoid [a] where
    mempty = []
    mappend = ++
```

## 2

```haskell
newtype Product a = Product { getProduct :: a} deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Product a) where
    mempty = 1
    Product x `mappend` Product y = Product (x * y)

getProduct $ Product 3 `mappend` Product 9
-- 27
```

## 3

```haskell
newtype Sum a = Sum { getSum :: a} deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Sum a) where
    mempty = 0
    Sum x `mappend` Sum y = Sum ( x + y)

getSum $ Sum 4 `mappend` Sum 5
-- 9

```

## 4

```haskell
newtype Any = Any { getAny :: Bool}
    deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid Any where
    mepty = Any False
    Any x `mappend` Any y = Any (x ||y)


getAny $ Any True `mappend` Any False
```

## 5

```haskell
newtype All = All { getAll :: Bool}
    deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid All where
    mepty = All True
    All x `mappend` All y = All (x && y)


getAll $ All True `mappend` All False
```

## 6

```haskell

-- Ordering = LT | EQ | GT

instance Monoid All where
    mepty = EQ
    mappend LT _  = LT
    mappend EQ y  = y
    mappend _ GT  = GT

import Data.Monoid

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend` (x `compare` y)
```

## 7

```haskell
newtype First a = First { getFirst :: Maybe a}
    deriving (Eq, Ord, Read, Show)

instance Monoid ( First a ) where
    mempty = First Nothing
    First (Just x) `mappend` _ = First (Just x)
    First Nothing `mappend` x = x
```

# using Monoid to fold

```haskell
import qualified Foldable as F

F.foldr :: F.Foldable t => (a -> b -> b) -> b -> t a-> b
F.foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m

```

## 1

```haskell
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance F.Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x `mappend`
                             F.foldMap f r


```

```haskell
testTree = Node 5
            (Node 3
                (Node 1 Empty Empty)
                (Node 6 Empty Empty)
            )
            (Node 9
                (Node 8 Empty Empty)
                (Node 10 Empty Empty)
            )

F.foldl (+) 0 testTree -- 42
F.foldl (*) 1 testTree -- 64800

getAny $ F.foldMap ( \x -> Any $ x == 3) testTree   -- Tree
getAll $ F.foldMap ( \x -> All $ x < 13) testTree   -- Tree
F.foldMap (\x -> [x]) -- [1,3,6,5,8,9,10]
```
