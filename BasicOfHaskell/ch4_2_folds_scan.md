## foldr , foldl, foldr1, foldl1

### 1

```haskell
sum' :: Num a => [a] -> a
```

```haskell
sum' = foldl (+) 0
```

### 2

```haskell
elem' :: Eq a => a -> [a] -> Bool
```

```haskell
elem' x = foldl (\z a -> (a == x) || z) False
```

### 3

```haskell
map' :: (a -> b) -> [a] -> [b]
```

```haskell
map' f = foldr (\a z -> f a : z) []
```

### 4

```haskell
maximum' :: (Ord a) => [a] -> a
```

```haskell
maximum' = foldl1 (\z a -> max z a)
```

### 5

```haskell
reverse' :: [a] -> [a]
```

```haskell
reverse' = foldl (\acc a -> a : acc) []
-- reverse' = foldl (flip (:)) []
```

### 6

```haskell
product' :: (Num a) => [a] -> a
```

```haskell
product' = foldl1 (*)
```

### 7

```haskell
filter' :: (a -> Bool) -> [a] -> [a]
```

```haskell
filter' f = foldr (\a acc -> if (f a) then a:acc else acc) []
```

### 8

```haskell
head' :: [a] -> a
```

```haskell
head' = foldr1 (\a _ -> a)
-- pattern
head' as = case as of
  [] -> error ""
  (x:_) -> x

```

### 9

```haskell
last' :: [a] -> a
```

```haskell
last' = foldl1 (\_ a -> a)
```

### 10

```haskell
dropWhile' :: (a -> Bool) -> [a] -> a
```

```haskell
dropWhile' p = foldr (\a acc -> if p a then acc else a:acc) []

```

## scanl, scanr, scanl1, scanr1

### some example

```haskell
scanl (+) 0 [1,2,3] -- == [0,1,3,6]

scanr (+) 0 [1,2,3] -- == [6,5,3,0]

scanl (flip (:)) [] [3,2,1]
-- [[], [3], [2,3], [1,2,3]]
```

### 1

```haskell
-- tails [1,2,3,4] == [[2,3,4],[3,4],[4],[]]
tails :: [a] -> [[a]]
```

```haskell
tails = scanr (:) [] . tail
```

### 2

```haskell
-- find idx :: accumlated sqrt > 10000

-- 1) infinite series of sqrt from 1
sqrts' = map sqrt [1..]

-- 2) accumulated series of sqrt
accSqrts' = scanl1 (+) (map sqrt [1..])

-- 3) drop count (<10000)
drops' = (length.takeWhile(<10000)) $ scanl1 (+) (map sqrt [1..])

-- answer
drop' + 1
```

## pointless style

omit argument style

###

```haskell
oddSquareSum :: Integer
oddSquareSum = sum ( takeWhile (<10000) (filter odd (map (^2) [1..])))
oddSquareSum = sum . takeWhile( <10000) . filter odd . map (^2) $ [1..]

```
