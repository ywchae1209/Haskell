# Fuctor

## Functor type-class

```haskell
class Functor f where
    fmap :: a -> b -> f a -> f b
    (<$>) = fmap
```

### Functor law

    fmap id == id
    id fmap == id

## 1. IO functor

````haskell
instance Functor IO where
    fmap = \f io -> (>>=) io (\a -> f a)
```haskell


```haskell
-- example

import Data.Char
import Data.List

-- getLine :: IO String

fmap (  map toUpper ) getLine
fmap (  reverse . map toUpper ) getLine
fmap (  intersperse '-' . reverse . map toUpper ) getLine

-- pointfree
\string -> (intersperse '-' ( reverse ( map toUpper string)))

````

## 2. function as Functor

(->) r  
:: function taking r

```haskell
instance Functor ((->) r) where
    fmap = (.)      -- fmap = \fab fra r -> fab( fra r)
```

```haskell
-- example
    (+3) <$> (*5) $ 10 -- ==  (+3) . (*5) $ 10 == 53
```

- note) when f :: r -> a

```haskell
    (->) r :: Functor
    r ->   :: contra_Functor -- see later
```

## 3. Count-Maybe

```haskell
data CountMaybe a = CountMaybe a Int | CountNothing deriving (Show)

--- counting fmap-call-count
instance Functor CountMaybe where
    fmap _ CountNothing =  CountNothing
    fmap f (CountMaybe a n) = CountMaybe ( f a) (n+1)
```

```haskell

-- example
    (++) <$> (Maybe "hello")      -- Option ( String -> String)
    compare <$> (Maybe "hello")   -- Option ( String -> Ordering)
    (*) <$> [1,2,3,4]             -- [Integer -> Integer]
    (\f -> f 9) . (*) $ [1,2,3,4] -- [9,18,27,36]
```

# Applicative Functor

## Applicative type=class

```haskell
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f ( a  -> b) -> f a -> f b
```

## 1. Option Applicative

```haskell
data Option a = Some a | None
instance Function Option where
    (<$>) f o = case o of
            | None -> None
            | Some a -> Some ( f a)

instance Fuctor Option where
    pure = Some
    (<*>) None _       = None
    (<*>) (Some f) sa  = fmap f sa
```

```haskell
    (++) <$> Some "Hello" <*> Some "World" -- Some "HelloWorld"
```

## 2 [] Applicative

```haskell
instance Functor [] where
    fmap f la = [f x | x <- la]

instance Applicative [] where
    pure = []
    ff <*> fa = ff >>= (\f -> fmap f fa)
```

- <pre> f <$> x <*> y <*> z ... </pre>

1. be familiar with syntatic sugar
2. be familiar with point-free style

```haskell

    [(+2),(-2)] <*> [1,2,3]
    -- [3,4,5,-1,0,1]

    [ (+), (*)] <*> [1,2] <*> [3,4]
    -- [4,5,5,6,3,4,6,8]

    (++) <$> ["ha","heh","hmm"] <*> ["?","!","."]
    --["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]

    (*) <$> [1,2,3] <*> [4,5,6]
    -- [4,5,6, 5,10,15, 6,12,18]

    filter (>10) $ (*) <$> [1,2,3] <*> [4,5,6]
    -- [15,12,18]

    (,,) <$> [1,2] <*> [3,4] <*> [5,6]
    -- [(1,3,5),(1,3,6),(1,4,5),(1,4,6),(2,3,5),(2,3,6),(2,4,5),(2,4,6)]

```

## 3 IO Applicative

```haskell
instance Functor IO where
    fmap f io = io >>= (\a -> f a)

instance Applicative IO where
    pure = return
    fa <*> a = fa >>= (\f -> fmap f a)
```

```haskell
    (++) <$> getLine <*> getLine    :: IO String
    (,)  <$> getLine <*> getLine    :: IO (String, String)
    (,,) <$> getLine <*> getLine <*> getLine    :: IO( String, String, String)
```

## 4 Funcion Applicative

```haskell
instance Functor ((->) r) where
    fmap = (.)  -- fmap fab fra = fab . fra

instance Applicative ((->) r) where
    pure a = \_ -> a
    f <*> g = \r -> f r (g r)   -- f :: r -> a -> b
```

```haskell

    (,,) <$> (+3) <*> (*2) <*> (/2) $ 5

    -- ( , , )
    --          . (+3)
    --                 . (*2)
    --                         . (/2)
    --              5       5       5
    -- (8.0,10.0,2.5)

```

```haskell

    (+) <$> (+3) <*> (*100)  --- ???

    {-
        (+) <$> (+3)            = (+) (a+3) :: 숫자를 주면 함수가 나온다.
        (*100)                  =     (a*100):: 숫자를 주면 숫자가 나온다.
        (+) <$> (+3) <*> (*100) = \a -> (+) (a+3) (a*100)
    -}

    (+) <$> (+3) <*> (*100) $ 5

-- https://stackoverflow.com/questions/31152042/haskell-evaluation-of-3-100-5
-- in detail
    (((+) <$> (+3))         <*> (*100)) $ 5        -- Add parens
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

```

## 5 ZipList Applicative : `newtype` keyword

list <\*> by position.

### `newtype` keyword

newtype :: single-constructor with single-field.

when we want to just take one type and wrap it in something to present it as another type.
`newtype` is faster than `data` keyword

```haskell

newtype ZipList a = ZipList { getZipList :: [a] }   -- data ZipList a = ZipList { getZipList :: [a] }

instance Functor ZipList where
    fmap f la = [f x | x <- la]

instance Applicative ZipList where
    pure x = ZipList (repeat x)     -- infinite List
    ZipList fs <*> ZipList xs = ZipList ( zipwith (\f x -> f x) fs xs)  -- apply f, x by position-wide

```

```haskell

    (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"
    -- [('d','c','r'),('o','a','a'),('g','t','t')]
```

## `liftA2`, `liftA3` :: Lift Applicative n

```haskell

liftA  :: Applicative f => (a -> b) -> (f a -> f b)
liftA f a  = f <$> a

liftA2 :: Applicative f => (a -> b -> c) -> (f a -> f b -> f c)
liftA2 f a b = f <$> a <*> b

liftA3 :: Applicative f => (a -> b -> c -> d) -> (f a -> f b -> f c -> fd)
liftA3 f a b c = f <$> a <*> b <*> c
```

```haskell

(\x -> [x]) <$> Just 4                      -- Just [4]

(:) (Just 3) (Just [4])                     -- Just [3,4]

liftA2 (,) Just "hello" Just "world"        -- ("hello", "world")

```

## `sequence`

```haskell
sequenceA :: (Traversable t, Monad a) => t (m a) -> m (t a)
-- Traversable :: iterate over, merge
-- Applicative :: liftA2
```

```haskell
sequenceA :: Applicative f => [f a] -> f [a]

sequenceA [] = f []
sequenceA [x:xs] = liftA2 (:) x xs

-- sequenceA [x:xs] = (:) <$> x <*> xs
-- sequenceA = foldr (liftA2 (:)) (pure [])     ; point-free version

```

```haskell
sequenceA [ getLine, getLine, getLine]      -- sequenceA [IO String]
heyh
ho
woo
["heyh","ho","woo"]                         -- IO [String]

sequenceA[(+3), (+4), (-)5, (/6)]           -- sequenceA[ a::Num -> a]
-- f = \(a::Num) -> [Num]

```

# Monoid

## Monoid type-class

```haskell
class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
    (<>) = mappend

    mconcat :: [m] -> m
    mconcat = foldr mappend mempty
```

## Some Monoids

- List Monoid

```haskell
instance Monoid [a] where
    mempty = []
    mappend = ++
```

- Num-product Monoid

```haskell

newtype Product a = Product { getProduct :: a} deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Product a) where
    mempty = 1
    Product x <> Product y = Product (x * y)

getProduct $ Product 3 <> Product 9         -- 27
```

- Num-Sum Monoid

```haskell
newtype Sum a = Sum { getSum :: a} deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Sum a) where
    mempty = 0
    Sum x <> Sum y = Sum ( x + y)

getSum $ Sum 4 <> Sum 5                      -- 9

```

- Bool-Any Monoid

```haskell
newtype Any = Any { getAny :: Bool} deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid Any where
    mepty = Any False
    Any x <> Any y = Any ( x || y )


getAny $ Any True <> Any False
```

- Bool-All Monoid

```haskell
newtype All = All { getAll :: Bool} deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid All where
    mepty = All True
    All x <> All y = All (x && y)

getAll $ All True <> All False
```

- Ordering-Monoid

```haskell

-- Ordering = LT | EQ | GT
instance Monoid Ordering where
    mepty = EQ
    mappend LT _  = LT
    mappend EQ y  = y
    mappend GT _  = GT

import Data.Monoid

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) <> (x `compare` y)
```

- First-Maybe Monoid

```haskell
newtype First a = First { getFirst :: Maybe a} deriving (Eq, Ord, Read, Show)

instance Monoid ( First a ) where
    mempty = First Nothing
    First (Just x) <> _ = First (Just x)
    First Nothing  <> x = x
```

# Using Monoid to fold

```haskell

foldr :: oldable t => (a -> b -> b) -> b -> t a-> b
foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
```

## 1

```haskell
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x l r) = foldMap f l <> f x <> foldMap f r
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

foldl (+) 0 testTree -- 42
foldl (*) 1 testTree -- 64800

getAny $ foldMap ( Any . ( == 3) )testTree   -- Tree
getAll $ foldMap ( All . ( < 13) ) testTree   -- Tree
foldMap (:[]) -- [1,3,6,5,8,9,10]
```
