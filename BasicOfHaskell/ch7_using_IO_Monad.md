# Some IO

```haskell

putStrLn :: String -> IO ()
putStr :: String -> IO ()
putChar :: Char -> IO ()

getChar :: IO Char
readLine :: IO String
print :: Show a => a -> IO ()


```

## 1

```haskell

-- main :: IO ()
-- putStrLn :: String -> IO ()
-- () is empty tuple, it's type is ()

main = putStrLn "hello, world"

```

## 2

```haskell

-- do == scala's for-comprehention
-- return  == scala's yield

main = do
    putStrLn "Hello, what's your name?"
    name <- getLine     -- getLine :: IO String
    putStrLn ("Hey " ++ name ++ ", how are you doing today?")      -- IO ()
    _ <- putStrLn ("Hey " ++ name ++ ", what's up today?")         -- this is same

```

## 3

```haskell

  main = do
    line <- getLine
    if null line
      then return ()                    -- return when empty-line
      else do
        putStrLn $ rev line
        main                            -- loop

  rev :: String -> String
  rev = unwords . reverse . words

```

# Some monadic cominator

## 1. sequence :: t ( m a ) -> m ( t a)

```haskell

--- sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)

main = do
    l <- sequence [ getLine, getLine, getLine]
    print l

```

```haskell
ghci> sequence $ map print [1,2,3,4]
1
2
3
4
[(),(),(),()]
```

## 2. mapM :: (a -> m b) -> t a -> m (t b)

```haskell

-- mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
-- map and seqeuence

ghci> mapM print [1,2,3,4]
1
2
3
4
[(),(),(),()]
```

### 3. forM :: t a -> (a -> m b) -> m (t b)

```haskell
ghci> forM [1,2,3,4] print
1
2
3
4
[(),(),(),()]

```

```haskell
-- forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
import Control.Monad

f = forM[ 1,2,3,4] (\n -> do
    putStr $ show n + " : "
    c <- getLine
    return c)

ghci> :t f
f:: IO [String]

ghci> f
1 : this
2 : is
3 : good
["this", "is", "good"]

ghci> do cs <- f ; mapM putStrLn cs
1 : this
2 : is
3 : good
this
is
good
[(),(),()]
```

## 4 forever

```haskell
import control.Monad
-- forever :: Applicative f => f a -> f b
-- forever a   = let a' = a *> a' in a'

ghci> forever $ do putStr "name :" ; n <- getLine ; putStr "address :" ; a <- getLine ; putStrLn $ n ++ " living in " ++ a

name :alice
address :wonderland
alice living in wonderland
name :paul
address :no where
paul living in no where
....

```

## when

```haskell
import Control.Monad
-- when :: applicative f => Bool -> f () -> f ()
main = do
    c <- getChar
    when( c /= ' ') $ do
        putChar c
        main
```
