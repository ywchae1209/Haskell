## Monad

```haskell
class Monad m where
    return x :: m x
    ( m a ) >>= f :: m a -> (a -> m b) -> m b


## Monad :: funcion as Monad ;;; working

instance Monad ((->) r) where
    return x = (\_ -> x)
    ma >>= f = f (ma r)             -- bind
    f <$> ma = \r -> f (ma r)       -- fmap
    ma <*> mb = do                  -- apply
        f <- ma
        x <- mb
        return (f x)

    -- ma <*> mb = ma >== \f -> f <$> mb

chain1 = (*2) >>= \a -> (+10) >>= \b -> return (b a)
chain1 = do
    a <- (*2)
    b <- (+10)
    return (b a)

```
