# Data.List

```haskell
import Data.List
import Data.Function

-- import in ghci
-- :m + Data.List

let

    a0 = listToMaybe "hello"                         -- Just 'h'  ; headOption ; Data.Maybe

    a1 = intersperse '.' "hello"
    a2 = intercalate ".." ["hello", "world", "spring"]
    a3 = transpose [[1,2,3], [4,5,6], [7,8,9]]
    a4 = concat ["con", "cat", "list"]
    a5 = and $ map odd [1,2,3]
    a6 = or $ map even [1,2,3]
    a7 = any (==4) [1.10]
    a8 = all (`elem` ['A'..'Z'] ) "Good Bye"

    a9 = take 10 $ iterate (*2) 1
    (a, b) = splitAt 3 "haskell"       -- ("has", "kell")
    a10 = takeWhile ( /= ' ') "This is a sentence"
    a11 = dropWhile ( /= ' ') "This is a sentence"
    (c, d) = span (/= ' ') "This is a sentence"
    (e, f) = break ( == ' ') "This is a sentence"

    a12 = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]

    a13 = head $ dropWhile (\(v, y, m, d) -> v < 1000) a12
    a14 = sort "This will be sorted soon"
    a15 = group "1111222233222567"    -- ["1111","2222","33","222","5","6","7"]
    a16 = inits "string"              -- ["","s","st","str","stri","strin","string"]
    a17 = tails "string"              -- ["string","tring","ring","ing","ng","g",""]

    a18 = "cat" `isInfixOf` "i'm a cat" -- contains
    a19 = "cat" `isPrefixOf` "i'm a cat"
    a20 = "cat" `isSuffixOf` "i'm a cat"

    a21 = 'a' `elem` "this is haskell"
    a22 = 'a' `notElem` "this is haskell"

    a23 = partition (`elem` ['A'..'Z']) "Upper and Lower to Tuple" -- ("ULT","pper and ower to uple")

    a24 = filter (`elem` ['A'..'Z'] ) "This Is A Sentence"

    a25 = find (`elem` ['A'..'Z'] ) "This Is A Sentence"    -- Just 'T'

    a26 = 4 `elemIndex` [2..10]                        -- Just 2
    a27 = 'e' `elemIndices` "This is a sentence"      -- [11,14,17]
    a28 = (`elem` [1..5]) `findIndex` [2..10]                -- Maybe
    a29 = (`elem` [1..5]) `findIndices` [2..10]              -- [Int]
    a30 = zip3 "this" "is" "good"
    a31 = zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,6,7] [2,4,6,9,11]

    a32 = lines "first line\n second line\n thrid line\n fourth"
    a33 = unlines a32
    a34 = words "this is haskell\ttest new\tlanguage"
    a35 = unwords a34

    a36 = nub "this is unique.."                      -- distinct
    a37 = delete 'h' "language name is haskell"

    a38 = "He is just a big baby" \\ "really?"      -- \\ difference ; just once,  every occurrence.
    a39 = "He is just a big baby" `union` "really?"       -- union
    a40 = "He is just a big baby" `intersect` "really?"       -- intersect
    a41 = insert 'x' "where is y insertion poisiton?"  -- act as sorted insert
    a42 = "where is y insertion poisiton?" !! 10     -- index access

    -- genericLength vs Length
    a43  = let xs = [1..10] in sum xs / length xs         -- error ; length xs :: Int
    a44  = let xs = [1..10] in sum xs / genericLength xs  -- ok    ; genericLength xs :: (NUm i)

```

```haskell
    -- Some generic
    genericLength           :: (Num i) => [a] -> i                     -- result
    genericDrop             :: (Integral i) => i -> [a] -> [a]         -- argument
    genericIndex            :: (Integral i) => [a] -> i -> a
    genericReplicate        :: (Integral i) => i -> a -> [a]
    genericSplitAt          :: (Integral i) => i -> [a] -> ([a], [a])
    genericTake             :: (Integral i) => i -> [a] -> [a]

    -- by nub(distinct) delete union intersect group
    nubBy                   :: (a -> a -> Bool) -> [a] -> [a]
    deleteBy                :: (a -> a -> Bool) -> a -> [a] -> [a]
    unionBy                 :: (a -> a -> Bool) -> [a] -> [a] -> [a]
    intersect               :: (Eq a) => [a] -> [a] -> [a]
    groupBy                 :: (a -> a -> Bool) -> [a] -> [[a]]
    sortBy                  :: (a -> a -> Ordering) -> [a] -> [a]
    insertBy                :: (a -> a -> Ordering) -> a -> [a] -> [a]
    maximumBy               :: Foldable t => (a -> a -> Ordering) -> t a -> a
    minimumBy               :: Foldable t => (a -> a -> Ordering) -> t a -> a

    -- import Data.Function
    -- on :: ( b -> b -> c) -> (a -> b) -> a -> a -> g,
    a45 = groupBy( (==) `on` (> 0)) [1..10]
    a46 = sortBy( compare `on` length) [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]]
```

## some equivalent

### 1. scala's split

```haskell
-- scala's split

  splitBy :: (a -> Bool) -> [a] -> [[a]]
  splitBy p s  =  case break p s of
                    (l, []) -> l : []
                    (l, (_:s')) -> l : splitBy p s'

  sp' :: Eq a => a -> [a] -> [[a]]
  sp' c = sp' (==c)

-- see meaning of ~
  splitBy' :: (a -> Bool) -> [a] -> [[a]]
  splitBy' p s  =  cons ( case break p s of
                      (l, []) -> (l, [])
                      (l, (_:s')) -> (l, splitBy' p s') )
                      where cons ~(h, t) =  h : t
```

### 1. scala's groupBy

```haskell
-- with Ord,
-- todo 1) :: Ord k --> Eq k
-- todo 2) :: use HashMap for construct
import Data.List
import Data.Function

scalaGroupBy :: Ord k => (v -> k) -> [v] -> [(k, [v])]
scalaGroupBy f = map ( \vs -> ((f . head) vs, vs) )
                    . groupBy ((==) `on` f)
                    . sortBy ( compare `on` f)
```

# Data.Char

```haskell
  import Data.Char

  -- predicates
  -- pred :: Char -> Bool

  isControl -- checks whether a character is a control character.
  isSpace -- checks whether a character is a white-space characters. That includes spaces, tab characters, newlines, etc.
  isLower -- checks whether a character is lower-cased.
  isUpper -- checks whether a character is upper-cased.
  isAlpha -- checks whether a character is a letter.
  isAlphaNum -- checks whether a character is a letter or a number.
  isPrint -- checks whether a character is printable. Control characters, for instance, are not printable.
  isDigit -- checks whether a character is a digit.
  isOctDigit -- checks whether a character is an octal digit.
  isHexDigit -- checks whether a character is a hex digit.
  isLetter -- checks whether a character is a letter.
  isMark -- checks for Unicode mark characters. Those are characters that combine with preceding letters to form latters with accents. Use this if you are French.
  isNumber -- checks whether a character is numeric.
  isPunctuation -- checks whether a character is punctuation.
  isSymbol -- checks whether a character is a fancy mathematical or currency symbol.
  isSeparator -- checks for Unicode spaces and separators.
  isAscii -- checks whether a character falls into the first 128 characters of the Unicode character set.
  isLatin1 -- checks whether a character falls into the first 256 characters of Unicode.
  isAsciiUpper -- checks whether a character is ASCII and upper-case.
  isAsciiLower -- checks whether a character is ASCII and lower-case.
```

```haskell
  -- convert
  -- to :: Char -> a

  toUpper -- converts a character to upper-case. Spaces, numbers, and the like remain unchanged.
  toLower -- converts a character to lower-case.
  toTitle -- converts a character to title-case. For most characters, title-case is the same as upper-case.

  digitToInt -- converts a character to an Int. To succeed, the character must be in the ranges '0'..'9', 'a'..'f' or 'A'..'F'.
  intToDigit -- is the inverse function of digitToInt. It takes an Int in the range of 0..15 and converts it to a lower-case character.
  ord -- convert characters to their corresponding numbers and vice versa:
  chr -- convert characters to their corresponding numbers and vice versa:
```

## some example

### 1

```haskell

  -- some examples.
  -- words "this is haskell code"
  filter ( not . any isSpace) . groupBy ( (==) `on` isSpace) "this is me"

```

### 2.

```haskell

  map intToDigit  . map digitToInt $ "1234abcd"
  map chr . map ord $ "abcdefgg"

```

### 3. Caesar ciper

```haskell
  -- example
  -- Caesar cipher
  encode :: Int -> String -> String
  encode sh = map chr . map (+sh) . map ord

  decode :: Int -> String -> String
  decode sh  = encode ( negate  sh )

```

# Data.Map

```haskell
  -- Map-like by List
  find' :: (Eq k) => k -> [(k, v)] -> v
  find' k = snd . head . filter ( (== k).fst )

  find'' :: (Eq k) => k -> [(k, v)] -> Maybe v
  find'' k m =
    do
      s <- find ( (==k).fst ) m
      return (snd s)
```

```haskell
  -- add dependency in package.yaml
  --      dependencies:
  ---         containers    <-- for Data.Map


  -- qualified :: for name conflict ; ex) filter exists in both List and Map
  import qualified Data.Map as Map
```

```haskell
  -- function's on Data.Map

  fromList :: Ord k => [(k, a)] -> Map k a
  fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> Map k a
    -- merge values of duplicated key with f ( a -> a -> a)

  insert :: Ord k => k -> a -> Map k a -> Map k a
  insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
    -- merge values of duplicated key with f ( a -> a -> a)

  null -- checks if a map is empty.
  size -- reports the size of a map.
  member -- contains key

  singleton -- takes a key and a value and creates a map that has exactly one mapping.

  lookup --  Just something (found) or Nothing
  map
  filter
  toList
  keys    -- keys == (map fst . toList )
  elems   -- values == (map snd . toList)

```

```haskell
-- some example
  phoneBook =
      [("betty","555-2938")
      ,("bonnie","452-2928")
      ,("patsy","493-2928")
      ,("lucille","205-2928")
      ,("wendy","939-8282")
      ,("penny","853-2492")
      ]

  m = Map.fromList phoneBook

  l = phoneBook ++ phoneBook
  m0 = Map.fromListWith (\v1 v2 -> v1 ++ ", " ++  v2) l
  m1 = Map.fromListWith max l
  m2 = Map.insertWith max "betty" "333-1111" m1

```

# Data.set

```haskell
  -- add dependency in package.yaml
  --      dependencies:
  ---         containers    <-- for Data.Map

  -- qualified :: for name conflict ; ex) filter exists in both List and Map

  import qulified Data.Set as Set
```

```haskell
  -- some functions
  fromList
  intersection
  difference
  union
  null
  size
  member
  empty
  singleton
  insert
  delete
  map
  filter
  toList
```

```haskell
text1 = "I just had an anime dream. Anime... Reality... Are they so different?"
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"
set1 = Set.fromList text1 -- fromList " .?AIRadefhijlmnorstuy"
set2 = Set.fromList text2 -- fromList " !Tabcdefghilmnorstuvwy"

set1 `Set.difference` set2 -- fromList ".?AIRj"
```

# Make a custom module

## 1

````haskell
-- Geometry.hs
module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidArea
, cuboidVolume
) where

sphereVolume :: Float -> Float
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)

sphereArea :: Float -> Float
sphereArea radius = 4 * pi * (radius ^ 2)

cubeVolume :: Float -> Float
cubeVolume side = cuboidVolume side side side

cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = rectangleArea a b * c

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b  ```
````

```haskell
import Geometry -- or
import Geometry (sphereArea, cubeArea) -- something like that..

```

## 2

```haskell
--- Geometry\Sphere.hs
module Geometry.Sphere
( volume
, area
) where

volume :: Float -> Float
volume radius = (4.0 / 3.0) * pi * (radius ^ 3)

area :: Float -> Float
area radius = 4 * pi * (radius ^ 2)

-- Geometry\Cuboid.hs
module Geometry.Cuboid
( volume
, area
) where

volume :: Float -> Float -> Float -> Float
volume a b c = rectangleArea a b * c

area :: Float -> Float -> Float -> Float
area a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b

-- Geometry\Cube.hs
module Geometry.Cube
( volume
, area
) where

import qualified Geometry.Cuboid as Cuboid

volume :: Float -> Float
volume side = Cuboid.volume side side side

area :: Float -> Float
area side = Cuboid.area side side side
```

### 1

```haskell
import Geometry.Sphere
```

### 2

```haskell
-- qualified as .... because volume, area may conflict in below
import qualified Geometry.Sphere as Sp
import qualified Geometry.Cuboid as Cubo
import qualified Geometry.Cube as Cu

```
