# Monad

a monoid in the category fo endo-functors

# type-classes : Fuctor, Applicative, Monad, Monoid

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b -- <$>
    <$> = fmap

class Functor f => Applicative f where
    pure :: a -> f a
    <*> :: f ( a -> b) -> f a -> f b

class Applicative m => Monad m where
    return :: a -> m a
    return = pure
    (>>=) :: m a -> (a -> m b) -> m b
    (>>)  :: m a -> m b -> m b
    (>>) a b = a >>= \_ -> b
    fail :: STring -> m a
    fail msg = error msg
    (<=<) :: (b -> m c) -> (a -> m b) -> a -> m c
    (<=<) f g a = g a >>= f

class Monoid m where
    mempty :: m
    <> :: m -> m -> m
```

## Some Monad

- Maybe Monad

```haskell
instance Functor Maybe where
    <$> f (Just a) = Just (f a)
    <$> _ _ = Nothing

instance Applicative Maybe where
    <*> (Just f) a = f <$> a
    <*> _ _ = Nothing
    pure = Just

instance Applicative Maybe => Monad Maybe where
    return = pure
    (>>=) (Just a) f = f a
    (>>=) _ _ = Nothing
    fail _ = Nothing
```

## `do` notation

- Maybe

```haskell
-- Just "3!"
foo :: Maybe String
foo = Just 3   >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))
```

```haskell
-- Just "3!"
foo :: Maybe String
foo = do
    x <- Just 3
    y <- Just "!"
    return (show x ++ y)
```

- List

```haskell
instance Monad [] where
    return x = [x]
    (>>=) xs f = concat (map f xs)
    fail _ = []
```

```haskell
[3,4,5] >>= \x -> [x, -x] -- [3,-3,4,-4,5,-5]
```

```haskell
liftA2 (,) [1,2] "ab"

[1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)


do
    n <- [1,2]
    ch <- ['a','b']
    return (n, ch)

-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
```

## Some Exercise :: Walk the line

```haskell
type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Bird -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs ( (left +n) - right ) < 4 = Just (left+n, right)
    | otherwise = Nothing

landRight :: Bird -> Pole -> Maybe Pole
landRight n (left, right)
    | abs ( (right +n) - left ) < 4 = Just (left, right+n)
    | otherwise = Nothing

ll = landLeft
lr = landRight
init i j = Just (i, j)

routine = do
    start <- init 0 0
    step1 <- lr 2 start
    step2 <- ll 4 step2
    lr 3 step2

-- you may think ... State+Maybe Monad and sequence

```

# Monoid of Monad

- note) Free-Monad

```haskell
class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a
```

## 1

```haskell
instance MonadPlus [] where
    mzero = []
    mplus = (++)
```

```haskell
guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

guard (  5 > 2 ) :: Maybe ()        -- Just ()
guard (  1 > 2 ) :: Maybe ()        -- Nothing
guard (  5 > 2 ) :: [()]            -- [()]
guard (  1 > 2 ) :: [()]            -- []

sevenOnly :: [Int] -> [Int]
sevenOnly xs = do
    x <- xs
    guard ( '7' `elem` show x)      -- show x :: 71 --> "71"
    return x

sevenOnly [1..50]                   -- [7,17,27,37,47]
```

## 2 Knight move in chess

### 1

```haskell
type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos ]

moveKnight (x, y) = do
    (x', y') <- [
        (x+2, y-1),
        (x+2, y+1),
        (x-2, y-1),
        (x-2, y+1),
        (x+1, y-2),
        (x+1, y+2),
        (x-1, y-2),
        (x-1, y+2) ]
    guard ( x' `elem` [1..8] && y' `elem` [1..8])
    return (x`, y`)
```

```haskell
-- list version
moveKnight (x, y) = filter onBoard
    [
        (x+2, y-1),
        (x+2, y+1),
        (x-2, y-1),
        (x-2, y+1),
        (x+1, y-2),
        (x+1, y+2),
        (x-1, y-2),
        (x-1, y+2) ]
    where onBoard (x', y') = ( x' `elem` [1..8] && y' `elem` [1..8])
```

### 2

```haskell
thirdMoves :: KnightPos -> [KnightPos ]
thirdMoves start = do
    first  <- moveKnight start
    second <- moveKnight first
    moveKnight second

-- how to make n-th moves-list ?
-- sequence ( not applicative sequence, but monad sequence)
-- moves n pos = foldl (>>=) [pos] (take n . repeat moveKnight)

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start target = tartget `elem` thridMoves start

(6, 2) `canReachIn3` (6,1) -- True
(6, 2) `canReachIn3` (7,3) -- False
```

## 3. Monad laws

```haskell
-- Left identity
    return x >>= f === f x

-- Right identity
    m >>= return === m

--- Associativity
    (m >>= f) >>= g === m >>= (\x -> f x >== g)
```

## 4. Kleisli composition

```haskell

(<=<) :: (b -> m c) -> (a -> m b) -> m c    -- Kleisli composition
(<=<) f g = (\a -> g a >>= f)
-- similar to .
-- (.) :: ( b -> c) -> (a -> b) -> (a -> c)
-- (.) f g = (\a -> f (g a) )

ghci> let f x = [x,-x]
ghci> let g x = [x*3,x*2]
ghci> let h = f <=< g
ghci> h 3
[9,-9,6,-6]


-- identity
    f <=< return === f
    return <=< f === f

--- Associativity
f <=< ( g <=< h ) === ( f <=< g ) <=< h
```
