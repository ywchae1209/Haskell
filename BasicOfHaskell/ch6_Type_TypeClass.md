# ADT (Algebraic Data Types)

## 1. co-product type

```scala
sealed trait Bool
case object True extends Bool
case object False extends Bool
```

```haskell
data Bool = True | False deriving (Show, Eq)
```

## 2.

```scala
sealed trait Shape
case class Circle ( x: Float, y: Float, r: Float)
case class Rectangle ( x: Float, y: Float, w: Float, h: Float)
```

```haskell
-- :t Circle :: Float -> Float -> Float -> Shape
-- :t Rectangel :: Float -> Float -> Float -> Float -> Shape
data Shape = Circle Float Float Float |
             Rectangle Float Float Float Float deriving (Show, Eq)

-- record syntax
data Shape' = Circle { x:: Float, y:: Float r:: Float} |
              Rectangle { x:: Float, y::Float, w:: Float h:: Float } deriving (Show, Eq)

```

## 3. Export

```haskell
-- Shape.hs
module Shape
 ( Circle
 , Rectangle
 )
```

## 4. deriving :: Short note on type-class

```haskell
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)


-- deriving :: means following instances are implicitly created.

-- these instances are type-classes instance.
-- type-classe :: virtual interface
-- instance    :: concrete instance implementing type-class's interface
--------------------------------------------------------------------------------
-- instance Eq Day where ...
-- instance Ord Day where ...
-- instance Show Day where ...
-- instance Read Day where ...
-- instance Bounded Day where ...
-- instance Enum Day where ...

Monday == Tuesday           -- False            ;; Eq

Monday `compare` Tuesday    -- LT               ;; Ord

show Wednesday              -- "Wednesday"      ;; Show

read "Tuesday"              -- Tuesday          ;; Read

minBound :: Day             -- Monday           ;; Bounded
maxBound :: Day             -- Sunday           ;; Bounded

succ Monday                 -- Tuesday          ;; Enum
pred Sunday                 -- Saturday         ;; Enum
[Monday..Sunday]            --                  ;; Enum

[minBound..maxBound] :: [Day]   -- Bounded, Enum

```

```scala
sealed trait Day extends Eq with Ord with Show with Read with Enum with Bounded
case object Monday extends Day
case object Tuesday extends Day
case object Wednesday extends Day
case object Thrusday extends Day
case object Friday extends Day
case object Satday extends Day
case object Sunday extends Day
```

## 5. type alias

```haskell
type String = [Char]        -- just alias, not new-real-type

type IntMap a = Map Int a
type IntMap = Map Int       -- pointless style; omit argument


phoneBook :: [(String,String)]
phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]

-----------------------------------------------------
type PhnoeNumber = String
type Name = String
type PhoneAddress = (String, String)
type PhoneBook = [(String, String)]
phoneBook' :: PhoneBook
phoneBook' =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]

inPhoneBook  :: Name -> PhoneAddress -> PhoneBook -> Bool
inPhoneBook name number pbook = (name, number) `elem` pbook
```

## 6. type parameter

### 1

```scala
sealed trait Maybe[T]
case class Just[T] ( value: T) extends Maybe[T]
case object Nothing extends Maybe[Nothing]
```

```haskell
data Maybe a = Just a | Nothing         --  a :: type-parameter
```

### 2

```scala
sealed trait Either[L,R]
case class Left[L,Nothing] ( value: L ) extends Either [L, Nothing]
case class Right[Nothing, R] ( value: R ) extends Either [Nothing, R]
```

```haskell
data Either l r = Left l | Right r      -- l, r :: typa-parameter
                deriving ( Eq, Ord, Read, Show)

ghci> :t Left True
Left True :: Either Bool a                   -- 'a' can by any type-parameter, partially applied type-constructor

ghci> :t Right "this is test"
Right "this is test" :: Either a [Char]      -- 'a' can by any type-parameter, partially applied type-constructor

```

### 3

```haskell

import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Show, Eq)
type LockerId = Int
type Code = String
type LockerMap = Map LockerId (LockerState, Code)

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]

lookup :: LockerId -> LockerMap -> Either String Code
lookup id map = case Map.lookup id map of
    Nothing -> Left $ "Locker Number : " ++ show id ++ " not exist!"
    Just (Taken, code ) -> Left $ "Locker Number : " ++ show id ++ " is already taken!"
    Just (Free, code )  -> Right code
```

## 7. recurive data structure

### 1

```scala
// scala
sealed trait List[T]
case class Cons[T]( head: T, tail: List[T]) extends List[T]
case object Nil extends List[Nothing]
```

```haskell
-- record syntax
List a = Cons { head :: a, tail:: List a} | Nil deriving (Show, Read, Eq, Ord)

ghci> 3 `Cons` (4 `Cons` (5 `Cons` Nil))     -- `Cons` :: infix call of Cons
Cons 3 (Cons 4 (Cons 5 Nil))

```

### 2

```haskell
infixr 5 :::                                    -- right-associative infix op.
data List a = Nil | a ::: (List a) deriving( Show, Read, Eq, Ord)

ghci> 3 ::: 4 ::: 5 ::: Nil
(:::) 3 ((:::) 4 ((:::) 5 Nil))               -- 4 + 5 === (+) 4 5 ;; prefix call of infix operator


infixr 5 +++                                    -- imitate ++ of [a]
(+++) :: List a -> List a -> List a

Nil +++ ys = ys
(x ::: xs) +++ ys = x ::: (xs +++ ys )

ghci> let a = 3 ::: 4 ::: 5 ::: Nil
ghci> let b = 6 ::: 7 ::: Nil
ghci> a .++ b
(::: 3 ((::: 4 ((::: 5 ((::: 6 ((::: 7 Nil))))
```

### 3

```haskell
data Tree a = Nil | Node a (Trea a) (Tree a) deriving (Show, Read, Eq)
```

```haskell
singleton a :: a -> Tree a
singleton a = Node a Nil Nil

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x Nil = singleton x
treeInsert x (Node a left right)
    | x == a -> Node x left right
    | x < a -> Node a (treeInsert x left) right
    | x > a -> Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ Nil = False
treeElem x (Node a left right)
    | x == a -> True
    | x < a -> treeElem a left
    | x > a -> treeElem a right
```

```haskell
ghci> let nums = [8,6,4,1,7,3,5]
ghci> let numTree = foldr treeInsert Nil nums
ghci> numsTree
Node 5 (Node 3 (Node 1 Nil Nil) (Node 4 Nil Nil)) (Node 7 (Node 6 Nil Nil) (Node 8 Nil Nil))
```

## 8. Type class

compare with ad-hoc polymorphism ( inhertiance in OOP )

### 1

```haskell
-- custom co-product type
data TrafficLamp = Red | Green | Yellow

-- type-class Eq with type-parameter a
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)

-- Eq instance for our custom co-product type
instance Eq TrafficLamp where
    (==) Red Red = True
    (==) Green Green = True
    (==) Yellow Yellow = True
    (==) _ _ = False

-- type-class Show with type-parameter a
class Show a where
    show :: a -> String

instance Show TrafficLamp where
    show Red = "Red light"
    show Green = "Green light"
    show Yellow = "Yellow light"


ghci> Red == Red
True

ghci> Red `elem` [Red, Green]
True

ghci> [Red,Green ]
[Red light, Green ling]

```

### 2. context bound with =>

```haskell
data Maybe a = Just a | Nothing

-- Eq for Maybe a
class (Eq a) => Eq (Maybe a) where
    (==) Nothing Nothing = True
    (==) (Just x) (Just y) = x == y  -- is possible by context-bound '(Eq a)=>'
    (==) _ _ => False
```

### 3. context bound with =>

```haskell
data Either l r = Left l | Right r

-- Eq for Either a b
class (Eq l, Eq r) => Eq (Either l r) where     -- Multiple context-bound
    (==) (Left x) (Left y) = x == y
    (==) (Right x) (Right y) = x == y
    (==) _ _ => False
```

## 9. Functor type-class

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b          -- fmap :: functor's map
```

### 1

```haskell
--- custom
data List a = Cons a (List a) | Nil

instance Functor' List where
    fmap _ Nil = Nil
    fmap f (Cons h t) = Cons (f h) (fmap f t)
```

### 2

```haskell
--- custom
data Maybe a = Just a | Nothing

instance Functor' Maybe where
    fmap _  Nothing = Nothing
    fmap f  (Just a) = Just $ f a
```

### 3

```haskell
--- custom
data Tree a = Nil | Node a (Tree a) (Tree b)

instance Functor' Tree where
    fmap _  Nil = Nil
    fmap f  (Node a left right) = Node (f a) (fmap f left) (fmap f right)
```

### 4

```haskell
--- custom
data Either a b = Left a | Right b

-- fmap :: (b -> c) -> Either a b -> Either a c
--         (b -> c) -> (Either a) b -> Either a c
instance Functor' (Either a) where   -- Either a :: Partially applied type-constructor, with free-parameter 'a'
    fmap f (Right x) = Right $ f x
    fmap f (left x) = Left x


type Eihter' b a = Eihter a b
-- fmap :: (a -> c) -> Either a b -> Either c b
--         (a -> c) -> (Either' b) a -> Either c b
instance Functor' (Either' b) where
    fmap f (Right x) = Right x
    fmap f (left x) = Left $ f x


```

### 5

````haskell
import qualified Data.Map as M

-- Map k v
instance Functor' (Map k) where
    fmap = M.map

## Kind

### 1. kind
```haskell
-- any proper type is king *
ghci> :k Int
Int :: *

-- type constructor with single type parameter
ghci> :k Maybe
Maybe :: * -> *

--- applied
ghci> :k Maybe Int
Maybe Int :: *


-- type constructor with two type parameter
ghci> :k Either
Either :: * -> * -> *
````

### 2

```haskell
-- Functor take type-constructor
ghci> :k Functor
Functor :: (* -> *) -> Constraint

```

### 3

```haskell

-- What kind of Student ?
data Student a b = Student { attr:: b a } deriving (Show)
```

```haskell
ghci> t: Student ( Just "name" )
Student ( Just "name" ) :: Student [Char] Maybe


ghci> t: Student ( Tree 'a' Nil Nil )
Student ( Tree 'a' Nil Nil ) :: Student Char Tree

ghci> t: Student ( "got it!" )
Student ( "got it!" ) :: Student Char []

ghci> k: Student
Student :: k -> (k -> *) -> *
```

### 4

```haskell
-- tricky and useless example : re-ordering type-parameter
class Tofu tc where
    tofu :: c a -> tc a c
    -- c :: * -> *                      ; c ~ container
    -- a :: *                           ; a ~ certain type
    -- tc :: * -> ( * -> *) -> *        ; tc ~ type-constructor

instance Tofo Student where
    tofu x = Student x

```

```haskell
gchi> tofu (Just 'a') :: Student Char Maybe
Student {attr = Just 'a'}

ghci> tofu ["HELLO"] :: Student [Char] []
Student {attr = ["HELLO"]}

```

### 5

```haskell
data Fruit c a b  = Fruti { attr1:: b, attr2:: c a }

```

```haskell
gchi> :k Fruit
Fruit :: (* -> *) -> * -> * -> *
```

```haskell
instance Functor' (Fruit c a) where
    fmap f (Fruit {attr1= x, attr2= y }) = Fruit { attr1 = f x, attr2 = y}
```
