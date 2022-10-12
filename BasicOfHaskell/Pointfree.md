# Pointfree

## Pointfree Style

Function defined **without actual parameters**

- **Pointfree** means pointless(headless) of lambda (ex 𝜆𝑥.𝑥𝑧 )

- [Pointfrees (Tacit) programming](https://en.wikipedia.org/wiki/Tacit_programming)

  > a programming paradigm in which function definitions do not identify the arguments (or "points") on which they operate. Instead the definitions merely compose other functions, among which are combinators that manipulate the arguments. A.K.A, **Tacit programming** ( tacit means unspoken. )  
  > Opposite :: **Pointful**(or pointed) style

- [haskell pointfree: https://wiki.haskell.org/Pointfree](https://wiki.haskell.org/Pointfree)

- [Some mathematics : Combinatory logic https://en.wikipedia.org/wiki/Combinatory_logic](https://en.wikipedia.org/wiki/Combinatory_logic)
  > Combinatory logic is a notation to eliminate the need for quantified variables in mathematical logic. It was introduced by Moses Schönfinkel and Haskell Curry, and has more recently been used in computer science as a theoretical model of computation and also as a basis for the design of functional programming languages.

> https://stackoverflow.com/questions/22437812/pattern-in-point-free-combinator-how-related-to-ski-calculus

## Combinatory logic

Full understanding is not necessary, but for the completeness of document

### 1. SKI combinator calculus

SKI combinator calculus  
[https://en.wikipedia.org/wiki/SKI_combinator_calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus)

> All operations in lambda calculus can be encoded via abstraction elimination into the SKI calculus as binary trees whose leaves are one of the three symbols S, K, and I (called combinators). note : SKI system is not practical for writing sofrware.
>
> [ combinators ]
>
> - S (Substitution) S x y z = x z ( y z)
> - K (const) K x y = x
> - I (Identity) I x = x

### 2. B,C,K,W system

[https://en.wikipedia.org/wiki/B,\_C,\_K,\_W_system](https://en.wikipedia.org/wiki/B,_C,_K,_W_system)

> The B, C, K, W system is a variant of combinatory logic that takes as primitive the combinators B, C, K, and W. This system was discovered by Haskell Curry

> B,C,K,W system
>
> - `B x y z = x (y z)` :: composition
> - `C x y z = x z y` :: swap
> - `K x y = x` :: const (discard second)
> - `W x y = x y y` :: duplicate

# Practices

## haskell's combinators

search "categorical combinator haskell"

```haskell
-- id
id :: a -> a
id a = a

-- combine
(.) :: ( b -> c)  -> (a -> b) -> a -> c
(.) f g a = f (g a)

-- flip
flip :: ( a -> b -> c) ->  b -> a ->  c
flip f b a = f a b

-- andThen
(&) :: ( a -> b) -> (b -> c) -> a -> c
(&) g f a = g ( f a)

-- const
const :: a -> b -> a
const a b = a

-- `on`
on :: ( b -> b -> c) -> (a -> b) -> a -> a -> c
on f m x y = f (m x) (m y)
```

## Basics

### 1. eta reduction

```haskell
-- pointful
sum' xs = foldr (+) 0 xs

-- pointfree ; eta reduction
sum = foldr (+) 0   -- sum :: (Foldable t, Num b) => t b -> b

```

### 2. dot(.) compos

```haskell

-- f,g,h 가 free-variable이고, single currry를 chaining 할 경우.
composed = (f . g. h)x = f(g(h x))

map f.g list
```

### 3. dot(.) compos

```haskell

orig = \a b c ->  a + (b * c)

-- 1. eleminiate c

-- f = (a+) section ; outer function
-- g = (b*) section ; inner function

f = (2+)
g = (3*)
pf0 = \c -> f ( g c)
    = \c -> (f.g) c
    = (f.g)

pf0 3 == ((2+).(3*)) 4 = 2+ (3* 4)

-- 2. g as param. ; parameterize inner function

-- suppose, g b c = (*) b c
-- f = (2+) ; certain outer function

pf1 = \f g b c -> f ( g b c)
    = \f g b c -> (f . ( g b ) ) c
    = \f g b   -> f . ( g b )
    = \f g b   -> ((f .).  g) b
    = \f g     -> (f .).g
    = \f       -> (f .).      -- B(Bf) ; B == (.)
    = \f       -> BBBf        -- B(Bf)  == BBBf
    =          -> BBB
    =             (.).(.)       -- known as Black-bird

pf1 (2+) (*) 3 4 = (2+) ( (*) 3 4)

-- Black-bird
B1 = \a b c d -> a ( b c d)
   = (.).(.)
   = (a.).b

-- 3. f as param. ; parameterize outer function

-- suppose, f a x = (+) a x
-- g = (3*) ; certain inner function

g = (3*)
pf1 = \f a g c -> (f a)( g c)
    = \f a g c -> ((f a).g) c
    = \f a g   -> (f a).g
    = \f a     -> (f a).
    = \f a     -> B(f a)
    = \f a     -> (B.f) a
    = \f       -> B.f       --- (.).f == BBf
    = \f       -> BBf
               -> BB
    =          (.).

pf1 (2+) (*) 3 4 = (2+) ( (*) 3 4)

-- Black-bird
B1 = \a b c d -> a ( b c d)
   = (.).(.)
   = (a.).b

-- 3. f,g as param. ; parameterize inner & outer function

pf3 = \f a g b c -> (f a) ( g b c)
pf3 = \f a g b c -> (f a) ( g b c)
    = \f a g b c -> ((f a).( g b)) c
    = \f a g b   -> (f a).( g b)
    = \f a g b   -> (((f a).). g) b
    = \f a g     -> ((f a).). g
    = \f a g     -> (((f a).).) g
    = \f a       -> ((f a).).     -- B(B(f a)) == BBB(f a)
    = \f a       -> BBB(f a)      -- BBB(f a) == ( B(BBB)f ) a
    = \f a       -> (B(BBB)f) a   -- BBB(f a) == ( B(BBB)f ) a
    = \f         -> B(BBB)f       -- BBB(f a) == ( B(BBB)f ) a
    =             -> ((.).(.)).     -- knwon as Eagle

pf3 (+) 2 (*) 3 4 = ((+) 2) ((*) 3 4)

--Eagle
E  = \a b c d e ->  (a b) ( c d e) -- == a b (c d e)
   = ((.).(.)).
   = ((.).(.)).a
   = ((.).(.))(a b)
   = ((.).(.))(a b)
   = ((a b).).c

```

\*\* Short Thinking ... in other lanuage (like scala)  
 scala에서 curry를 최소로 사용한다면  
 위의 사례는 inner-outer 처리를 parameter화 한 일반형

`process` = `outer-process` ( `inner-process` `target` )

`outer-process` = f (arg1, ... argn) ; outer-process is made from 1-curry
`inner-process` = g (arg1, ... argn) ; inner-process is made from 1-curry

<pre>

case 1

  outer = f ; fixed
  inner = g ; fixed

  proc3 = (.)        f g x
  proc2 = f.         g x
  proc1 = f . g      x

case 2

  outer = f ; fixed
  inner = g a ; inner-process is parameterized

  proc1 = (.)(.)(.)  f g a x
  proc2 = (f.).      g a x
  proc3 = (f.).g     a x
  proc4 = (f.)(g a)  x

case 3

  outer = f a ; outer-process is parameterized
  inner = g ; fixed

  proc1 = (.)(.)     f a g x
  proc2 = (.).f      a g x
  proc3 = (f a).     g x
  proc4 = (f a).g    x

case 4

  outer = f b ; outer-process is parameterized
  inner = g a ; inner-process is parameterized

  proc1 = ((.)(.)(.)).            f b g a x
  proc2 = ((.)(.)(.)).f           b g a x
  proc3 = ((.)(.)(.))(f b)        g a x
        = ((f b).).)              g a x
  proc4 = (f b).).g               a x
  proc5 = (f b).(g a)             x

</pre>

### 함수의 적용에 대해..

i spent couple of days... to find out the basics... ㅠㅠ

<pre>
1. 함수의 적용

   1. 가장 높은 우선순위

      f a + g b c d e = (f a) + (g b c d e)
      a b c . d e f g = (a b c) . (d e f g)

   2. 왼쪽부터 적용 ( function apply is left assiciative )

      f a b c d = ((((f a) b) c) d)

2. 괄호안의 결과가 함수일 경우
   ( some-caluculation ) a b c
   괄호안의 결과가 함수 일 경우, 그 함수에 a b c를 순차적으로 적용하는 것임.

   (f.) a b
   (f.) == (.) 은 3항 함수, 1개가 partial-applied 된 것. take-2 function.
   (f.) a == take-1 function
   ((f.) a)b == f (a b)

   (.f) a b
   (.f) == take 2
   (.f) a == a.f
   (.f) a b == (a.f)b == a(f b)

3. 고차함수의 반환값
   f :: a -> b -> c -> d -> e 일때,
      f a :: b -> (c -> d -> e) ;; 당연하게도

4. (.) 는  right-associative
   f . g . h . i  == f .( g. ( h. (i))  

</pre>

## further reading

https://hackage.haskell.org/package/data-aviary-0.2.3/docs/Data-Aviary-Birds.html  
http://pointfree.io/
