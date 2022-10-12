## Zipper

- zipper vs lens ?

https://stackoverflow.com/questions/22094971/what-are-the-differences-between-lenses-and-zippers

...about updating data-structure...

Zippers are akin to cursors: they allow to traverse trees in an ordered manner. Their usual operations are `up`, `down`, `left`, `right` and `edit`. (names may vary depending on impl)

Lenses are some sort of generalized keys (as in "keys of an associative datastructure"). The structure does not need to be ordered. Their usual operations are `fetch` and `putback` and are very similar to get and assoc. (names may vary depending on impl)

So as you see zippers are very much concerned about hierarchy (up/down) and order (left/right) while lenses are just about focusing (hence the name) on a piece of data, which may even be a projection (that is something that didn't existed on its own in the original structure).

## Tree

```haskell

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

freeTree :: Tree Char
freeTree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )
```

### walk through

```haskell
chngeToP :: Tree Char -> Tree Char  -- 'W' --> 'P'
changeToP (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r)
```

```haskell
data Direction = L | R deriving Show
type Directions = [Direction]

---
elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _ ) = elemAt ds l
elemAt (R:ds) (Node _ _ r ) = elemAt ds r
elemAt [] (Node x _ _ ) = x


---
changeToP :: Directions -> Tree Char  ->  Tree Char
changeToP (L:ds) (Node x l r ) = Node x (changeToP ds l) r
changeToP (R:ds) (Node x l r ) = Node x l (changeToP ds r)
changeToP [] (Node _ l r )     = Node 'P' l r

---
let newTree = changeToP [R,L] freeTree
elemAt [R,L] newTree
-- 'P'

```

### trail of walk

```haskell
---
type Breadcrumbs = [Direction]

---
goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goLeft (Node _ l _ , bs ) = (l, L:bs)

---
goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goRight (Node _ _ r , bs ) = (r, R:bs)

(-:) x f = f x

---
(freeTree, []) -: goRight -: goLeft
-- (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty),[L,R])

```

### going back

```haskell
---
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving Show
type Breadcrumbs a = [Crumb a]

---
goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)

goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight (Node x l r, bs) = (l, RightCrumb x l:bs)


goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp (t, LeftCrumb x r:bs) =  (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)

```

## Zipper

```haskell
---
type Zipper a = (Tree a, Breadcrumbs a)

---
modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty, bs)

-- move & modify
let newFocus = (freeTree,[]) -: goLeft -: goRight -: modify (\_ -> 'P')
let newFocus2 = newFocus -: goUp -: modify (\_ -> 'X')

--
attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

-- move & attach
let farLeft = (freeTree,[]) -: goLeft -: goLeft -: goLeft -: goLeft
let newFocus = farLeft -: attach (Node 'Z' Empty Empty)

--
topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost z = topMost (goUp z)

```

- exercise :: List case

```haskell

data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

type LZipper a = ([a], [a])

forwarad :: LZipper a -> LZipper a
forwarad (x:xs, bs) = (xs, x:bs)

backward :: LZipper a -> LZipper a
backward (xs, b:bs) = (b:xs, bs)

```

```haskell
let xs = [1,2,3,4]
    f1 =(xs,[]) -: forware -- ([2,3,4], [1])
    f2 = f1 -: forware     -- ([3,4], [2, 1])
    f3 = f2 -: backward    -- ([2,3,4], [1])
```

## one more Zipper : Maybe

```haskell

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r:bs)
goLeft (Empty, _) = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node x l r, bs) = Just (r, RightCrumb x l:bs)
goRight (Empty, _) = Nothing

goUp :: Zipper a -> Maybe (Zipper a)
goUp (t, LeftCrumb x r:bs) = Just (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = Just (Node x l t, bs)
goUp (_, []) = Nothing

let coolTree = Node 1 Empty (Node 3 Empty Empty)

return (coolTree,[]) >>= goRight
-- Just (Node 3 Empty Empty,[RightCrumb 1 Empty])

return (coolTree,[]) >>= goRight >>= goRight
-- Just (Empty,[RightCrumb 3 Empty,RightCrumb 1 Empty])

return (coolTree,[]) >>= goRight >>= goRight >>= goRight
-- Nothing

```
