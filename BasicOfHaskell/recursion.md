# Recursion Exercise

_Woring..._

## 1 max

```haskell
max' :: (Ord a) => [a] -> a
```

### solution

```haskell
-- guard
max' xs
    | [] <- xs = error "maximum on empty"
    | [x] <- xs = x
    | (h:t) <- xs =  max x (max' xs)

-- case
max' xs =
    case xs of
        [] -> error "maximum on empty"
        [x] -> x
        (h:t) ->  max x (max' t)
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
