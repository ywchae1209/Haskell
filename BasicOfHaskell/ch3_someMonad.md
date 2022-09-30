## List Monad

```haskell
data List a = Nil | Cons a (List a)        -- co-product type definition

instance (Show a) => Show (List a) where    -- typeClass for print List
    show Nil = ""
    show (Cons x xs) = show x ++ ", " ++ show xs

instance Functor List where                 -- map
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Monad List where
    return x = Cons x Nil                   -- pure     return :: a -> [a]
    xs >>= k = join $ fmap k xs             -- flatMap  k :: a -> [b]

join :: List (List a) -> List a             -- flatten
join Nil = Nil
join (Cons xs xss) =  cat xs (join xss)

cat :: List a -> List a -> List a           -- concat two list
cat Nil ys = ys
cat (Cons x xs) ys = Cons x (cat xs ys)

neighbors :: (Num a) => a -> a -> List a    -- some function that makes a list
neighbors x dx = Cons (x - dx) (Cons x (Cons (x + dx) Nil))


test = do                                   -- monadic combination
    x <- neighbors 0 100
    y <- neighbors x 1
    return y

main = print $ test
```

## Option Monad ( similar to scala' Option)

see: https://hackage.haskell.org/package/base-4.3.1.0/docs/src/Data-Maybe.html

```haskell
data Option a = Some a | None

instance (Show a) => Option a where
    show None = "None"
    show (Some a) = "Some (" ++ show a ++ ")"

instance Functor Option where
    fmap _ None = None
    fmap f (Some a) = Some $ f a

instance Monad Option where
    None >>= _ = None
    (Some a) >>= k = k a

    None >> _ =  None
    (Some _) >> k = k

    return = Some
    fail _ = None

isSome :: Option a -> Bool
isSome None = False
isSome _ = True

isNone :: Option a -> Bool
isNone None = True
isNone _ = False

fromSome :: Option a -> a
fromSome None = error "fromSome : None"
fromSome (Some a) = a

optionToList :: Option a -> [a]
optionToList None = []
optionToList (Some a) = [a]

listToOption :: [a] -> Option a
listToOption [] = None
listToOption (h:_) = Some(h)

catOptions :: [Option a] -> [a]
catOptions ls = [x | Some a <- ls]

mapOption :: (a -> Option b) -> [a] -> [b]
mapOption f [] = []
mapOption f (h:t) =
        case f h of
            None -> bs
            Some b -> b:bs
        where
            bs = mapOption f t

--- argument : default-value function option
some :: b -> (a -> b) -> Option a -> b
some b _ None  = b
some b f (Some a) = f a
```
