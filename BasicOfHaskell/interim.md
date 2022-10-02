```haskell

class Functor f where
    fmap    :: (a -> b) -> f a -> f b


data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance Functor Tree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

Fuctor's Law
    fmap id = id
    fmap ( f . g ) = fmap f . fmap g

```

```haskell

class Monad m where
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    return :: a -> m a
    fail   :: String -> m a

-- some exercise
    m >> k =        -- with bind( >>= )
                    m >>= \_ -> k

    do e1 ; e2 =    -- with >>
                    e1 >> e2


    do p <- e1 ; e2 = -- with >>, >>=
                    e1 >>= \p -> e2
                    e1 >>= (\v -> case v of
                                        p -> e2
                                        _ -> fail "e1"

    return a >> k           =  k a
    m >>= return            = m
    xs >>= return . f       = fmap f xs

    m >>= (\x -> k x >>= h) = m >>= k >>= h     -- is m >>= k >>= h

```

```haskell

class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

-- think [], list-concat
    m >>= \x -> mzero = mzero
    mzero >>= m       = mzero

    m 'mplus' mzero = m
    mzero 'mplus' m = m

[(x,y) | x <- [1,2,3], y <- [1, 2,3,4] ,x /= y]

do
    x <- [1,2,3]
    y <- [1,2,3,4]
    True <- return ( x /= y)        --
    return ( x, y)

[1,2,3] >>=
    ( \x ->
        [1,2,3,4] >>=
             (\y -> return (x /= y) >>=
             (\r -> case r of
                    True -> return (x, y)
                    False -> fail "") ) )

```

```haskell
-- special version of liftM1
lift1 :: (a -> b) -> [a] -> [b]
lift1 f as =
    as >>= return . f

liftM1 :: Monad m => (a -> b) -> m a -> m b
liftM1 f ma =
    ma >>= return . f

-- special version of liftM2
Lift2' :: (a -> b -> c) -> [a] -> [b] -> [c]
Lift2' f as bs =
    do
        a <- as
        b <- bs
        return (f a b)

-- cf) Control.Monad liftM2
liftM2 :: Monad m => ( a -> b -> c) -> m a -> m b -> m c
liftM2 =
    do
        a' <- a
        b' <- b
        return ( f a' b')

liftM2 :: Monad m => ( a -> b -> c) -> m a -> m b -> m c
liftM2 =
    do
        a' <- a
        b' <- b
        pure ( f a' b')

```

```haskell
-- State Monad

data St s a = St { run:: s -> (a, s)}

instance Functor (St s) where
  fmap f (St r) =               -- map
    St $ \s0 ->
      let
        (a, s1) = r s0
      in (f a, s1)

instance Applicative (St s) where
  pure x = St $ \s -> (x, s)   -- pure, return

  St sf <*> St sa =            -- apply
    St $ \s0 ->
      let
        (f, s1) = sf s0
        (a, s2) = sa s1
      in (f a, s2)

instance Monad ( St s) where
  St sa >>= f =               -- flatMap
    St $ \s0 ->
      let
        (a, s1) = sa s0
        St sb = f a
      in sb s1

readSt :: St s s
readSt = St $ \s -> (s, s)

updateSt :: ( s -> s) -> St s ()
updateSt f = St $ \s -> ((), f s)

runSM :: s -> St s a -> (a, s)
runSM s (St run)

```

```haskell
-- pointfree ; https://wiki.haskell.org/Pointfree

f1 x y = x + 1
f1 = const . ( 1+ )     -- const :: x -> y -> x

f0 x y = ( x, y )
f0 = flip (,)           -- flip :: (a -> b-> c) -> b -> a -> c

f (a, b) = ( b, a)
f = uncurry ( flip (,))


```
