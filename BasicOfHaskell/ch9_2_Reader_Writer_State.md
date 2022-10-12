##

# recap : Monad type-class

```haskell
class Functor f where
    <$> :: a -> b -> f a -> f b

class Functor f => Applicative f where
    pure :: a -> f a
    <*> :: f ( a -> b ) -> f a ->  f b

class Applicative m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    return = pure
    fail :: String -> m a
    (>>) :: m a -> m b -> m b
    (>>) a b = a >>= \_ -> b
    (<=<) :: (b -> m c) -> (a -> m b) ->  a -> m c
    (<=<) f g a = g a >>= f

class Monoid m where
    mempty :: m
    (<>) :: m -> m -> m

```

# Some Monads

## Writer Monad

`a -> (a, w)`

```haskell
newtype Writer w a = Writer { runWriter :: (a, w) }

instance Monoid w => Monad (Writer w) where
    return x = Writer( x, mempty)
    (>>=) (Writer (x, w) )f =
        let (Writer (y, w')) = f x
        in Writer ( y, w <> w')
    tell w = Writer((), w)

```

- ex1

```haskell
    runWriter (return 3 :: Writer String Int)  -- (3,"")
    runWriter (return 3 :: Writer (Sum Int) Int)  -- (3,Sum {getSum = 0})
    runWriter (return 3 :: Writer (Product Int) Int)  -- (3,Product {getProduct = 1})
```

- ex2

```haskell
logNumber :: Int -> Writer [String] Int
logNumber x = Writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    return (a*b)

-- (15,["Got number: 3","Got number: 5"])

```

- ex3

```haskell
-- prepend list with prepending function (for better perfomance)
-------------------------------------------------------------------
newtype PList a = PList { getMList :: [a] -> [a]}

instance Monoid PList where
    mempty = PList id
    PList m <> PList m' = PList ( m . m' )

toPList :: [a] -> PList a
toPList xs = PList (\xs -> xs++)

fromPList :: PList a -> [a]
fromPList (PList f) = f []

```

```haskell
-- 최대공약수
gcd' :: Int -> Int -> Writer (PList String) Int

gcd' a b
    | b == 0 = do
        tell (toPList ["Finished with " ++ show a] )
        return a
    | otherwise = do
        result <- gcd' b (a `mod` b)
        tell (toPList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
        return result
```

```haskell
f = runWriter ( gcd' 8 3)

fst f
-- 1

mapM_ putStrLn $ fromPList $ snd $ f    -- mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()

Finished with 1
2 mod 1 = 0
3 mod 2 = 1
8 mod 3 = 2

```

## State Monad

`s -> (a s)`

```haskell
newtype State s a = State { runState :: s -> (a, s)}

instance Monad (State s) where
    return a = State( \s -> (a, s))
    (>>=) State(sa) f = State \s ->
        let (a, s') = sa s
            State(g) = f a
        in g s'

get :: State s s
get    = State $ \s -> (s, s)

put :: State s ()
put s' = State $ \s -> ((), s')
```

- Stack State

```haskell
type Stack = [Int]

pop :: State Stack Int
pop = State $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push n = State $ \xs -> ((), n:xs)
```

```haskell
stackManip :: State Stack Int
stackManip = do
    push 3
    a <- pop
    pop

runState stackManip [5,8,2,1]
-- (5, [8,2,1])

stackStuff :: State Stack ()
stackStuff = do
    a <- pop
    if a == 5
        then push 5
        else do
            push 3
            push 8

runState stackStuff [9,0,2,1,0]
-- ((),[8,3,0,2,1,0])

moreStack :: State Stack ()
moreStack = do
    a <- stackManip
    if a == 100
        then stackStuff
        else return ()

resetStack :: State Stack ()
resetStack  = do
    current <- get
    if current == [1,2,3]
        then put [8,3,1]
        else put [ 9,2,1]

```

- RandomGen State

```haskell
import System.Random
import Control.Monad.State

random :: (RandomGen g, Random a ) => g -> (a, g)

randomSt :: ( GandomGen g, Random a) => State g a
randomSt = State random

coin3 :: State StdGen ( Bool, Bool, Bool)
coin3 = do
    a <- RandomSt
    b <- RandomSt
    c <- RandomSt
    return ( a, b, c)

runState coin3 (mkStdGen 33)

-- ((True,False,True),680029187 2103410263)
```

## Error handling : Either

```haskell
data Either e a = Left e | Right a

instance (Error e) => Monad (Either e) where
    return = Right
    (>>=) (Right a) f = f a
    (>>=) (Left e) _ = Left e
    fail msg = Left msg

```

# Useful Monadic Functions

## `liftM`

```haskell
liftM  :: (Monad m) => (a -> b) -> m a -> m b
liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM3 :: (Monad m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM4 :: (Monad m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
liftM5 :: (Monad m) => (a -> b -> c -> d -> e -> f) -> m a -> m b -> m c -> m d -> m e -> mf

liftM f m = m >>= (\a -> return (f a))


-- applicative
liftA  :: (Applicative f) => (a -> b) -> f a -> f b
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA3 :: (Applicative f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d

```

## `join`

scala's `flatten`

```haskell
join:: (Monad m) => m (m a) -> m a
```

## `filterM`

```haskell
filterM :: (Applicative m) => (a -> m Bool) -> [a] -> m [a]
```

```haskell
keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4 = do
        tell ["Keeping " ++ show x]
        return True
    | otherwise = do
        tell [show x ++ " is too large, throwing it away"]
        return False

fst $ runWriter $ filterM keepSmall [9,1,5,2,10,3]
-- [1,2,3]

mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3]
-- 9 is too large, throwing it away
-- Keeping 1
-- 5 is too large, throwing it away
-- Keeping 2
-- 10 is too large, throwing it away
-- Keeping 3
```

```haskell

-- element exist/not-exist pairs
powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs

powerset [1,2,3]
-- [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
```

## `foldM` (`foldl`)

```haskell
foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
```

- example RPN calculator

```haskell
-- "1 3 + 2 *" -> ["1","3","+","2","*"] -> fold

import Data.List

solveRPN :: String -> Double
solveRPN str = do
    [result] <- foldM foldingFunction [] (words st)
    return result

readMaybe :: (Read a) => a -> Maybe a
readMaybe str = case reads str of   -- type ReadS a = String -> [(a, String)]
        [(x,"")] -> Just x
        otherwise -> Nothing

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "+" = return ((x * y):ys)
foldingFunction (x:y:ys) "*" = return ((x * y):ys)
foldingFunction (x:y:ys) "-" = return ((y - x):ys)
foldingFunction xs numberStriring = liftM (:xs) (readMaybe numberString)
:w

```

## `<=<` (`.`) :: composing monadic function

```haskell

-- Knight move
-- in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight
-- canReachIn3 :: KnightPos -> KnightPos -> Bool
-- canReachIn3 start end = end `elem` in3 start

inMany :: Int -> KnightPos -> [KnightPos]
inMany n start = retrun start >>= foldr (<=<) return (replicate n moveKnight)

```

## Exercise

probability

```haskell

import Data.Ratio

-- Rational( 유리수) 1 % 4 == .25
newtype Prob a = Prob { getProb :: [(a, Rational)]} deriving Show

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

-- fmap negate (Prob [(3,1%2),(5,1%4),(9,1%4)])
-- Prob {getProb = [(-3,1 % 2),(-5,1 % 4),(-9,1 % 4)]}

instance Monad Prob where
    return x = Prob [ (x, 1%1)]
    m >>= f = flatten (fmap f m)
    fail _ = Prob []

-- join
flatten :: Prob ( Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob xs', p) = map (\(x, r) -> (x, p * r)) xs'


```

```haskell
data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads,1%2),(Tails,1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]

flipThree :: Prob Bool
flipThree = do
    a <- coin
    b <- coin
    c <- loadedCoin
    return (all (==Tails) [a,b,c])

-- getProb flipThree
-- [(False,1 % 40),(False,9 % 40),(False,1 % 40),(False,9 % 40),
--  (False,1 % 40),(False,9 % 40),(False,1 % 40),(True,9 % 40)]
```
