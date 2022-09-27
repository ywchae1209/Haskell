# Lambda Calculus

`from haskell-programming-from-first-principles (chapter 1)`

- **The lambda calculus**, a model of computation devised in the 1930s by Alonzo Church.
- **Functional programming** is a computer programming paradigm that relies on functions modeled on **mathematical functions**.
- programs are a combination of **expressions**, can be **reduced** or **evaluated**.

## Some concepts

- **pure function**
- **referential transparency**

## Lambda Calculus

    example of pure function
         𝑓(𝑥) = 𝑥 + 1

### lambda terms

**note) In below, x y z... are variables(term) separately.**

    𝜆𝑥.𝑥𝑧

    𝜆𝑥  : head, ( 𝜆 : start of head, . : end of head)
    x   : single parameter(__bound variable__)
    𝑥𝑧  : body (expression, z is __free variable__)

    𝜆𝑥𝑦.𝑥𝑦
      meaning: 𝑥 𝑦 is two bound variables. x maybe a function
      is equal to __𝜆𝑥.(𝜆𝑦.𝑥𝑦)__

    * __bound variable__
    * __free variable__
    * __combinator__ : lambda term with no free variables. (only combine given argument)

### Alpha equivalance

    below lambda terms are all the same thing. (obiously)

        𝜆𝑥.𝑥
        𝜆𝑑.𝑑
        𝜆𝑧.𝑧

### Beta reduction

    elemimate the head by applying bound variable.

        (𝜆𝑥.𝑥) 2
               2

    (𝜆𝑥.𝑥) is identity function

    * __normal form__ : shorten form of lambda term. (opposite to __diverge__)

### Omega

    omega is lambda term that diverges.

    (𝜆𝑥.𝑥𝑥)(𝜆𝑥.𝑥𝑥)
    --> (𝜆𝑥.𝑥𝑥)(𝜆𝑥.𝑥𝑥)

    (𝜆𝑥.𝑥𝑥𝑥)(𝜆𝑥.𝑥𝑥)
    --> (𝜆𝑥.𝑥𝑥)(𝜆𝑥.𝑥𝑥)(𝜆𝑥.𝑥𝑥)

## Exercise

### Combinator

    1. 𝜆𝑥.𝑥𝑥𝑥           : combinator
    2. 𝜆𝑥𝑦.𝑧𝑥           : not combinator (z)
    3. 𝜆𝑥𝑦𝑧.𝑥𝑦(𝑧𝑥)      : combinator
    4. 𝜆𝑥𝑦𝑧.𝑥𝑦(𝑧𝑥𝑦)     : combinator
    5. 𝜆𝑥𝑦.𝑥𝑦(𝑧𝑥𝑦)      : not combinator (z)

### Normal form or diverge?

    1. 𝜆𝑥.𝑥𝑥𝑥           : normal
    2. (𝜆𝑧.𝑧𝑧)(𝜆𝑦.𝑦𝑦)   : diverge ( repetitive reduction to self form.)
    3. (𝜆𝑥.𝑥𝑥𝑥)𝑧        : not normal form ( can be __zzz__ )

### Equivalance

    1. 𝜆𝑥𝑦.𝑥𝑧
        a) 𝜆𝑥𝑧.𝑥𝑧
        b) 𝜆𝑚𝑛.𝑚𝑧           <--
        c) 𝜆𝑧.(𝜆𝑥.𝑥𝑧)

    2. 𝜆𝑥𝑦.𝑥𝑥𝑦
        a) 𝜆𝑚𝑛.𝑚𝑛𝑝
        b) 𝜆𝑥.(𝜆𝑦.𝑥𝑦)
        c) 𝜆𝑎.(𝜆𝑏.𝑎𝑎𝑏)       <--

    3. 𝜆𝑥𝑦𝑧.𝑧𝑥
        a) 𝜆𝑥.(𝜆𝑦.(𝜆𝑧.𝑧))
        b) 𝜆𝑡𝑜𝑠.𝑠𝑡            <--
        c) 𝜆𝑚𝑛𝑝.𝑚𝑛

### Beta Reduction

    (𝜆𝑥.𝑥)(𝜆𝑦.𝑦)
          (𝜆𝑦.𝑦)            <--


    (𝜆𝑥.𝑥)(𝜆𝑦.𝑦)𝑧
                𝑧           <--

    (𝜆𝑥𝑦.𝑥𝑦)(𝜆𝑧.𝑎) 1
                a           <--

    1. (𝜆𝑎𝑏𝑐.𝑐𝑏𝑎)𝑧𝑧(𝜆𝑤𝑣.𝑤)          z
    2. (𝜆𝑥.𝜆𝑦.𝑥𝑦𝑦)(𝜆𝑎.𝑎)𝑏           bb
    3. (𝜆𝑦.𝑦)(𝜆𝑥.𝑥𝑥)(𝜆𝑧.𝑧𝑞)         (𝜆𝑧.𝑧𝑞)(𝜆𝑧.𝑧𝑞) --> 𝑞𝑞
    4. (𝜆𝑧.𝑧)(𝜆𝑧.𝑧𝑧)(𝜆𝑧.𝑧𝑦)         (𝜆𝑧.𝑧𝑦)(𝜆𝑧.𝑧𝑦) --> 𝑦𝑦
    5. (𝜆𝑥.𝜆𝑦.𝑥𝑦𝑦)(𝜆𝑦.𝑦)𝑦          𝑦𝑦
    6. (𝜆𝑎.𝑎𝑎)(𝜆𝑏.𝑏𝑎)𝑐             (𝜆𝑏.𝑏𝑎)(𝜆𝑏.𝑏𝑎)𝑐 --> 𝑎𝑎𝑐
    7. (𝜆𝑥𝑦𝑧.𝑥𝑧(𝑦𝑧))(𝜆𝑥.𝑧)(𝜆𝑥.𝑎)   (𝜆𝑧'.𝑧𝑎)     : hint) free, bound variable

## more resources

1. Raul Rojas. A Tutorial Introduction to the Lambda Calculus
   http://www.inf.fu-berlin.de/lehre/WS03/alpi/lambda.pdf
2. Henk Barendregt; Erik Barendsen. Introduction to
   Lambda Calculus
   http://www.cse.chalmers.se/research/group/logic/TypesSS05/Extra/geuvers.pdf
3. Jean-Yves Girard; P. Taylor; Yves Lafon. Proofs and Types
   http://www.paultaylor.eu/stable/prot.pdf
