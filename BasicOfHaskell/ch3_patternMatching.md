# Pattern mathing in function definition

Thess are simple, but do not underestimate and practice thoroughly.

## Guard

Pay attention to the position of the (=) mark.
( easy to make a mistake )

```haskell

-- haskell's guard (when)
-------------------------------------------------
 say n
   | n == 1 = "One"
   | n == 2 = "Two"
   | n == 3 = "threee"
   | n == 4 = "four"
   | n < 10 = "below ten"
   | otherwise = "many"

-- with where
-------------------------------------------------
 bmi :: RealFloat a => a -> a -> String
 bmi w h
    | idx <= skin = "under-weight"
    | idx <= normal = "normal"
    | idx <= fat = "fat"
    | otherwise = "whale"
    where
      idx = w / h ^ 2
      (skin, normal, fat) = (18.5, 25.0, 30.0)

-- simple example
-------------------------------------------------
 max' a b
    | a > b = a
    | otherwise = b

 -- with matching ( <- ) and boolean condition
-------------------------------------------------
 takeBigger2 :: Ord a => [a] -> [a] -> [a]
 takeBigger2 s1 s2
    | [] <- s1, l@(h:t) <- s2 = l
    | [] <- s2, l@(h:t) <- s1 = l
    | (h:t) <-s1, (h':t') <- s2, h > h' = h : takeBigger2 t t'
    | (h:t) <-s1, (h':t') <- s2, h < h' = h' : takeBigger2 t t'

-- with function call result and boolean condition
-- https://wiki.haskell.org/Pattern_guard
-------------------------------------------------

-- lookup :: (Eq k, Hashable k) => k -> HashMap k v -> Maybe v
 mergeResult env x y
    | Just a <- lookup x env, Just b <- lookup y env , x > y = Just $ x + y
    | otherwise = Nothing

```

## case ... of ...

```haskell
-- scala's pattern matching
--    (something) match {
--    case a =>
--    case b =>
--    case c =>
--    }

-- haskell's pattern matching
-- = case (something) of
--     (de-construction ) -> value
--     (de-construction ) -> value
--     where something = ....

-- matching
-------------------------------------------------
 tell ::  [a] -> String
 tell as =
    case as of
        [] -> "empty"
        (x:[]) -> "one"
        (x:_:[]) -> "two"
        (x:_:_:[]) -> "three"
        otherwise -> "many"

-- matching
-------------------------------------------------
 takeBigger :: Ord a => [a] -> [a] -> [a]
 takeBigger s1 s2 =
   case (s1, s2) of
     ([], l@(h:t)) -> l       -- as
     (l@(h:t), []) -> l       -- as
     ((h:t), (h':t')) -> (max h h') : takeBigger t t'

-- matching and guard
-------------------------------------------------
 takeBigger3 :: Ord a => [a] -> [a] -> [a]
 takeBiggere s1 s2 =
   case (s1, s2) of
     ([], l@(h:t)) -> l       -- as
     (l@(h:t), []) -> l       -- as
     ((h:t), (h':t')) | h > h'  -> h : takeBigger3 t t'
     ((h:t), (h':t')) | h' >= h -> h' : takeBigger3 t t'

-- matching and guard
-------------------------------------------------
 mergeResult2 env x y = case ( lookup x env, lookup y env) of
    (Just a, Just b) | a > b ->  Just $ x + y
    ( _, _ ) -> Nothing

```
