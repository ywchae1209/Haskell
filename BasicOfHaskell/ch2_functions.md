# Syntax about function

`from https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/function-application`

(객체지향 언어가 객체를 다루는 데에 효율적인 문법을 제공하듯이)
Haskell은 함수형 언어이므로, 함수를 정의하고 사용하는 데에 최소한의 노력을 들이도록 되어 있다.
다른 언어에서 볼 수 없었던 문법적 특징이어서 당혹스럽기 쉽고, 먼저 살펴보면 적잖은 호기심과 자극이 될 듯.

## ( 함수의 사용에서 ) **최소한** 이란 무슨 의미일까?

What does it mean "bare minimum"? It means that This:

### 함수호출

**일련의 식별자 나열은** 함수호촐이다.  
_any series of identifiers is a function call_ or, as we often call it, a _function application_.  
( 함수호출을 의미하는 괄호() 조차 없다. )

```haskell
    a b c d
```

- meaning  
   `a` is function  
   `b`, `c`, `d` : three argument  
   Wow, it's function call !!!

### 함수정의

try below in REPL.

```active haskell

--- definitions
a b c d = "Function a called with arguments " ++ b ++ " " ++ c ++ " " ++ d
b = "b"
c = "c"
d = "d"

--- usage
main = putStrLn ( a b c d )

```

#### Function Definition is Equation.

함수와 변수의 정의가 더이상 간결할 수 있을까?  
괄호나 다른 군더더기가 전혀없다. ( strong-type 언어인데, 타입도 추론한다.)  
( def, function 같은 키워드도 안 보인다. )

함수정의에서 = 기호를 사용한 것은 우연이 아니다.
함수가 사용된 모든 곳에 오른쪽의 expresion을 사용하거나 왼쪽의 함수정의를 언제든 치환할 수 있다.
( 순수함수, 참조투명성 )

### Currying

Below two are equivalent.

아래에서  
1과 2의 `f`의 타입은 `a -> b -> c` 정도일텐데  
 3에서 `f`의 타입은 `(a , b) -> c` 에 해당.

1.

```haskell
    f a b
```

and

2.

```haskell
    (f a b)
```

but, Below is not same. ( still valid Haskell.)

3.

```haskell
    f (a, b)
```

### Precedence (우선순위)

함수호출은 가장 높은 우선순위.

So how would you interpret this?

0.

```haskell
    a b * c d
```

1.

```haskell
    (a b) * (c d)
```

2.

```haskell
    a (b * c) d
```

- Exp0. is identical to Exp1, _not_ equivalent to Exp2.

### $ Notation

괄호를 줄이면 좋을 것 같은 상황....

Exp 0.

```haskell
    f ( g (h  i)))
    --- f, g, h is function, i is argument
```

Exp 1.

```haskell
    f g h i         -- == f ( g ( h i ) ) ; function call is right-associative
    --- is not equivalent to Exp0.
```

Exp 2.

```haskell
    f $ g $ h $ i
    --- is equivalent to Exp0.
    --- $ make function call precedence to lowest.
    --- $ : evaluate right first and-then apply it to left
```

---

```active haskell
sq x = x * x
main = print $ sq (sqrt (7 + 9))
```

```active haskell
sq x = x * x
main = print $ sq $ sqrt $ 7 + 9
```

`$` binds to the right, the square root will be executed first; and because it has **the lowest precedence**, the addition will be performed _before_ function application.

### Dot(.) notation

함수합성

Applying a function to the result of another function is called _function composition_ and has its own operator, the dot, `.`.  
This operator has **very high precedence**, surpassed only by by that of function application.

The composition of `sq` with `sqrt` may be written as `sq . sqrt`. This new function, when acting on a number will first take its square root and then square the result, so this will work too:

```active haskell
sq x = x * x
main = print $ (sq . sqrt) $ 7 + 9
```

### id function

The function that really does nothing is called the _identity_, `id`. Composing identity with any function doesn't change the behavior of that function. Try it:

```active haskell
sq x = x * x
main = print $ (sqrt . id) 256
```
