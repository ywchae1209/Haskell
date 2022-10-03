# Recursion Exercise

_Woring..._

## 1 max

```haskell
max' :: (Ord a) => [a] -> a
```

### solution

```haskell

-- with guard
max' xs
| [] <- xs = error ""
| [x] <- xs = x
| (h:t) <- xs = max h (max' t)

-- with case
max' xs = case xs of
[] -> error ""
[x] -> x
(h:t) -> max h (max' t)

--- using foldl1
max' xs = foldl1 max xs

```

## 2 rep (replicate)

```haskell
rep' :: (Num i, Ord i) => i -> a -> [a]
```

### solution

```haskell
-- guard
rep' i a
    | i <= 0 = []
    | otherwise = a : rep' (i -1) a

-- case
rep' i a =
    case i of
        n | n <= 0 -> []
        n | n > 0 -> a : rep' (n-1) a

-- if
rep' i a =
    if i <= 0
    then []
    else a : rep' (i-1) a
```

## 3 take n

```haskell
take' :: (Num i, Ord i) => i -> [a] -> [a]
```

### solution

```haskell
-- guard
take' n a
    | (h:t) <- a, n > 0 = h : take' (n-1) t
    | otherwise = []

-- case
take' n a =
    case n of
        (h:t) | n > 0 -> h : take' (n-1) t
        otherwise -> []
```

## 4 reverse

```haskell
reverse' :: [a] -> [a]
```

### solution

```haskell
reverse' a =
    case a of
        [] ->  []
        (x:xs) -> reverse' xs ++ [x]

```

## 5 repeat (infinte)

```haskell
-- infinite repeate
-- note: haskell's constructor is lazy

repeat' :: a -> [a]
```

### solution

```haskell
repeat' a = a : repeat' a

```

## 6 zip

```haskell
zip' :: [a] -> [b] -> [(a,b)]
```

### solution

```haskell
zip' a b =
    case (a, b) of
        ( [], _ ) -> []
        ( _, [] ) -> []
        ( h:t , h':t') -> (h, h'): zip' t t'

zip' a b
    | [] <- a = []
    | [] <- b = []
    | (h:t) <- a, (h':t') <- b = (h,h') : zip' t t'
```

## 7 elem

```haskell
elem' :: (Eq a) => a -> [a] -> Bool
```

### solution

```haskell

elem' e l
    | [] <- l = False
    | (h:t) <- l = h == e || elem' e t

elem' e l =
    case l of
        (h:t) -> h == e || elem' e t
        otherwise -> False

```

## 8 concat

```haskell
concat' :: [a] -> [a] -> [a]
```

### solution

```haskell
concat' a b
    | [] <- a = b
    | (h:t) <- a = h: concat' t b


concat' a b =
    case a of
        [] -> b
        (h:t) -> h: concat' t b
```

## 9 join

```haskell
-- flatten
join' :: [[a]] -> [a]
```

### solution

```haskell
-- O(N)
join' l =
    let cs = (map (\s -> concat' s) l)
        f = foldr (.) id cs
    in f []
```

```haskell
  --http://learnyouahaskell.com/recursion
  -----------------------------------------
  maximum' :: (Ord a)  => [a] -> a
  --------------------------------------

  maximum' [] = error ""
  maximum' [x] = x
  maximum' (h:t)
    | h > m = h
    | otherwise = m
    where m = maximum' t

  --------------------------------------
  replicate' :: Int -> a -> [a]
  --------------------------------------

  replicate' n a
    | n <= 0 = []
    | otherwise = a : replicate' (n-1) a

  --------------------------------------
  take' :: Int -> [a] -> [a]
  --------------------------------------

  take' n _ | n <= 0 = []
  take' n [] = []
  take' n (h:t)  = h : take' (n-1) t

  --------------------------------------
  drop' :: Int -> [a] -> [a]
  --------------------------------------

  drop' _ [] = []
  drop' n a | n <= 0 = a
  drop' n (x:xs) = drop' (n-1) xs

  --------------------------------------
  reverse' :: [a] -> [a]
  --------------------------------------

  reverse' [] = []
  reverse' (x:xs) = reverse' xs ++ [x]

  --------------------------------------
  reverse'' :: [a] -> [a]
  --------------------------------------

  reverse'' xs = foldl (\z x -> x:z ) [] xs

  --------------------------------------
  cons' :: [a] -> [a]
  --------------------------------------

  cons' xs = foldr (\x z -> x:z) [] xs

  --------------------------------------
  repeat' :: a -> [a]
  --------------------------------------

  repeat' a = a : repeat' a

  --------------------------------------
  zip' :: [a] -> [b] -> [(a, b)]
  --------------------------------------

  zip' [] _ = []
  zip' _ [] = []
  zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

  --------------------------------------
  zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
  --------------------------------------

  zipWith' f _ [] = []
  zipWith' f [] _ = []
  zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


  --------------------------------------
  elem' :: (Eq a) => a -> [a] -> Bool
  --------------------------------------

  elem' a [] = False
  elem' a (h:t) = a == h || elem' a t

  --------------------------------------
  quickSort' :: (Ord a) => [a] -> [a]
  --------------------------------------

  quickSort' [] = []
  quickSort' (x:xs) =
    small ++ [x] ++ big
    where
      small = quickSort' [a | a <- xs, a <= x]
      big = quickSort' [a | a <- xs, a > x]

  -- infinite fib series from 0
  fibs' :: [Int]
  -------------------------------------

  fibs' = 0:1: zipWith' (+) fibs' (tail fibs')

  -------------------------------------
  fib' :: Int -> Int
  -------------------------------------

  fib' n = head $ drop n fibs'

  -------------------------------------
  factorial' :: Int -> Int
  -------------------------------------
  factorial' n
    | n <= 1 = 1
    | otherwise = n * factorial' (n-1)

```
