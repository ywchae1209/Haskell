# Monad

```haskell
class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

    (<=<) :: (b -> m c) -> (a -> m b) -> m c    -- Kleisli composition
    (<=<) f g = (\a -> g a >>= f)
    -- similar to .
    -- (.) :: ( b -> c) -> (a -> b) -> (a -> c)
    -- (.) f g = (\a -> f (g a) )

    (>>) :: m a -> m b -> m b
    (>>) ma mb = ma >>= (\_ -> mb)

    pure = return
    (<*>) :: m ( a -> b) -> m a -> m b
    (<*>) mf ma = mf >>= (\f -> (ma >>= \a -> return (f a)))

    (<$>) :: ( a -> b) -> m a -> m b
    (<$>) f ma = ma >>= (\a -> return (f a))

    fail :: String -> m a
```

## recap

```haskell
(<$>) :: (Functor f) => (a -> b) -> f a -> f b          -- fmap
(<*>) :: (Applicative f) => f ( a -> b) -> f a -> f b   -- apply
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b          -- bind
```

## 1

```haskell
instance Monad Maybe where
    return x = Just x
    (>>=) Nothing _  = Nothing
    (>>=) (Just a) f = f a
    fail _ = Nothing


```

## do notation

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

## 2

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
[1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)

do
    n <- [1,2]
    ch <- ['a','b']
    return (n, ch)

-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
```

# Monoid of Monad

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
f <=< ( g <=<> h ) === ( f <=< g ) <=< h

```
