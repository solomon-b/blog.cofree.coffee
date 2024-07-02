---
author: Solomon Bothwell
title: Lensy Moore
---

## Morphisms in Poly

Morphisms in `Poly` are dependent lenses:

``` Agda
record _⇒_ (P Q : Poly) : Set where
  no-eta-equality
  field
    map-base : P .Base → Q .Base 
    map-fiber : (tag : P .Base ) → Q .Fiber (map-base tag) → P .Fiber tag
```

We can demonstrate non-dependent lenses quite clearly:

``` Agda
Lens : Set → Set → Set → Set → Set
Lens S T A B = monomial S T ⇒ monomial A B

view : ∀{S T A B : Set} → Lens S T A B → S → A
view lens s = lens .map-base s

over : ∀{S T A B : Set} → Lens S T A B → (A → B) → S → T 
over lens f s = lens .map-fiber s (f (lens .map-base s))

set : ∀{S T A B : Set} → Lens S T A B → B → S → T
set lens b s = lens .map-fiber s b

lens : ∀{S T A B : Set} → (S → A) → (S → B → T) → Lens S T A B
(lens get set) .map-base = get
(lens get set) .map-fiber = set

projₗ : ∀{A B : Set} → Lens (A × B) (A × B) A A
projₗ = lens proj₁ λ where
  (fst , snd) → λ a → (a , snd)

projᵣ : ∀{A B : Set} → Lens (A × B) (A × B) B B
projᵣ = lens proj₂ λ where
  (fst , snd) → λ b → (fst , b)
```

Morphisms of the form `Syˢ ⇒ Oyᴵ` are Moore Machines:

``` Agda
Moore : Set → Set → Set → Set
Moore S I O = S y^ S ⇒ O y^ I

moore : ∀{S I O : Set} → (S → O) → (S → I → S) → Moore S I O
moore output transition .map-base = output
moore output transition .map-fiber s = transition s
```

Wiring diagrams are poly maps `P ⇒ Q` where `Q` is the outer interface
of the diagram and `P` describes the interior mappings of the diagram.

``` Agda
Diagram : Poly → Poly → Set
Diagram P Q = P ⇒ Q
```

## Lensy Moore {#lensy-moore-1}

So how far can we get leveraging the `lens` library in Haskell to model
Moore Machines and Wiring Diagrams?

First off we need to overload `view` and `set` to get some more
polymorphism:

``` haskell
view :: Lens s t a b -> s -> a
view l s = getConst $ l Const s

set :: Lens s t a b -> s -> b -> t
set l s b = runIdentity $ l (\_ -> Identity b) s
```

With that out of the way we can define `Moore` as a type alias and use
`get` and `set` for our observation and transition functions:

``` haskell
-- | S × yˢ ⇒ O × yᴵ
type Moore s i o = Lens s s o i

observe :: Moore s i o -> s -> o
observe m s = view m s

transition :: Moore s i o -> s -> i -> s
transition m s i = set m s i
```

We can then define a recursive function for feeding inputs into a
`Moore` given an initial state:

``` haskell
runMoore :: Moore s i o -> s -> [i] -> [o]
runMoore _ s [] = []
runMoore m s (i:is) =
  let nextState = transition m s i 
      observation = view m s
  in observation : runMoore m nextState is
```

A simple latch machine to test this out:

``` haskell
-- | A Moore machine that sets its state to the max of the input ands
-- current state.
--
-- Int × y^Int => Int × y^Int
latchMachine :: Moore Int Int Int
latchMachine = lens id max
```

    > runMoore latchMachine 0 [1,2,3,4,5,4,3,2,1]
    [0,1,2,3,4,5,5,5,5]

Neat!

We can create some other classic examples from the [Poly
Book](https://github.com/ToposInstitute/poly/blob/pdf/poly-book.pdf):

``` haskell
-- | A memoryless dynamical system
--
-- oy^a => oy^a
mds :: (i -> o) -> Moore o i o
mds f = lens id (const f)

-- | Counter takes unchanging input and produces as output the
-- sequence of natural numbers 0, 1, 2, 3, ... .
--
-- Int × y^Int => Int × y^()
counter :: Moore Int () Int
counter = lens id (\n () -> n + 1)

-- | Int × y^Int => Int × y^(Int × Int)
plus :: Moore Int (Int, Int) Int
plus = lens id (\_ (x, y) -> x + y)

-- | Int × y^Int => Int × y^Int
delay :: Moore Int Int Int
delay = lens id (\x y -> y)
```

## Tensor Product

This encoding is cute but we can take it a bit further. `Poly` has 7
monoidal structures, one of which is the parallel product aka `tensor`
aka `_⊗_`:

``` Agda
infixr 7 _⊗_
_⊗_ : Poly → Poly → Poly
(P ⊗ Q) .Base  = Base P × Base Q
(P ⊗ Q) .Fiber (ptag , qtag) = Fiber P ptag × Fiber Q qtag
```

`tensor` takes the product of both the base and the fiber of polynomials
`P` and `Q`.

This translats quite nicely to our `Moore` lenses:

``` haskell
tensor :: Moore s i o -> Moore t i' o' -> Moore (s, t) (i, i') (o, o')
tensor m n =
  let observe' (s, t) = (observe m s, observe n t)
      transition' (s, t) (a, a') = (transition m s a, transition n t a')
  in lens observe' transition'
```

`tensor` also happens to be the `combine` operation of a 3 parameter
monoidal functor:

``` haskell
class Monoidal3 f where
  unital3 :: f () () ()
  combine3 :: (f x y z, f x' y' z') -> f (x, x') (y, y') (z, z')

newtype Moore' s i o = Moore' (Lens s s o i)

instance Monoidal3 Moore' where
  unital3 :: Moore' () () ()
  unital3 = Moore' ($)

  combine3 :: (Moore' s i o, Moore' t i' o')-> Moore' (s, t) (i, i') (o, o')
  combine3 (Moore' m, Moore' n) = Moore' (tensor m n)
```

But this requires an annoying `newtype` wrapper in Haskell so we will
skip the typeclass.

With `tensor` we can take two `Moore` machines and run them in parallel.

## Wiring Diagrams

Since `Poly` is a `Category` it has a composition operation that
composes `P ⇒
Q` and `Q ⇒ R` into `P ⇒ R`. We can visualize this using wiring diagrams

Given `g : P ⇒ Q` and `f : Q ⇒ R`:

    --
    --          ┌─────────┐
    --          │  ┌───┐  │ 
    --   g :  ──┼──┤  P├──┼───
    --          │  └───┘ Q│
    --          └─────────┘
    --
    --          ┌─────────┐
    --          │  ┌───┐  │ 
    --   f :  ──┼──┤  Q├──┼───
    --          │  └───┘ R│
    --          └─────────┘

We define composition as:

``` src
--
--               ┌───────────────┐
--               │  ┌─────────┐  │
--               │  │  ┌───┐  │  │ 
--   compose : ──┼──┼──┤  P├──┼──┼──
--               │  │  └───┘ Q│  │
--               │  └─────────┘ R│
--               └───────────────┘
```

So if `g` were a `Moore` machine `Syˢ ⇒ Byᴬ` then `f` would be some
`Poly Map` `Byᴬ ⇒ R` where `R` is some other polynomial. When you
compose these together you would get `Syˢ ⇒ R`.

`f : Byᴬ ⇒ R` is a wiring diagram and by composing it with `g` you are
giving yourself a new interface onto `g`. In this sense `Poly` is the
language of interface design.

In the full dependent world of `Poly` you can take this a lot further
but here we can still do some neat stuff. For example, we can create a
Fibonacci algorithm constructed out of a couple `Moore` machines and a
wiring diagram that dictates how to plug them together.

## Fibonacci

Our wiring diagram looks like this:

    --              ┌────────────────────────┐
    --              │  ┌───────┐             │ 
    --              │  │┌─────┐│  ┌─────┐    │ 
    --              │  └┤ℤ    ││  │     │    │ 
    -- fib-wire : ──┤   │  P ℤ├┴──┤ℤ Q ℤ├┬───┼──
    --            ()│  ┌┤ℤ    │   │     ││   │ℤ
    --              │  │└─────┘   └─────┘│   │ 
    --              │  └─────────────────┘   │ 
    --              └────────────────────────┘

Our outer interface receives a unit value and produces an integer.
Inside the diagram we have slots for two `Moore` machines wired together
in a particular fashion.

Every time the `fibonacci` machine receives a unit value, the output of
`P`--aka the `observation`--is fed into the `Q` machine and back into
`P`\'s input along with the observation from the `Q` machine. The
observation from the `Q` machine is also fed into the output of the
final fibonacci machine.

I had said that a wiring diagram is a map between two polynomials but
here we have two slots on the inside of the diagram which would imply
two polynomials.

This is where `tensor` comes into play. The product of two polynomials
is itself a polynomial and so the entire fibonacci map is
`(ℤ × ℤ) × y^((ℤ × ℤ) × ℤ) ⇒ ℤ
y^Unit`.

We can take the `plus` and `delay` Moore machines, `tensor` them
together, and then compose them with the fibonacci wiring diagram to
build our final fibonacci machine:

``` haskell
plusDelay :: Moore (Int, Int) ((Int, Int), Int) (Int, Int)
plusDelay = tensor plus delay

fibWiring :: Lens (Int, Int) ((Int, Int), Int) Int ()
fibWiring = 
  lens
    -- The delay output is the final observation:
    (\(pout, dout) -> dout)
    -- Input the plus result and the delay result back into the plus
    -- Input the plus result into the delay
    (\(pstate, dstate) () -> ((pstate, dstate), pstate))

fib :: Moore (Int, Int) () Int
fib = plusDelay . fibWiring
```

Notice how the getter and setter of `fibWiring` describes how to wire
the outputs of the inner machines to the new outer interface (the getter
function) and how to internally wire together the inputs and outputs of
the inner machines to one another (the setter function).

If we run this thing we get just what we expected:

    > runMoore fib (1, 0) [(), (), (), (), (), (), (), (), (), ()]
    [0,1,1,2,3,5,8,13,21,34]

Trippy!

## Moore-Mealy Pairings

As a bonus round here is `Mealy`:

``` haskell
type Mealy s i o = Lens (s, i) s o ()

observe' :: Mealy s i o -> (s, i) -> o
observe' m (s, i) = view m (s, i)

transition' :: Mealy s i o -> (s, i) -> s
transition' m (s, i) = set m (s, i) ()

runMealy :: Mealy s i o -> s -> [i] -> [(o, s)]
runMealy m s [] = []
runMealy m s (i:is) =
  let
    o = observe' m (s, i)
    s' = transition' m (s, i)
   in (o, s) : runMealy m s' is
```

There is a special relationship between `Moore` and `Mealy` where their
interfaces are a perfect fit for one another such that we can
\'annihilate\' them against one another:

``` haskell
annihilate :: (s, t) -> Moore s i o -> Mealy t o i -> void
annihilate (s, t) moore mealy = 
  let o = observe moore s
      i = observe' mealy (t, o)
      s' = transition moore s i
      t' = transition' mealy (t, o)
   in annihilate (s', t') moore mealy
```

We leveraged this idea in
[cofree-bot](https://github.com/cofree-coffee/cofree-bot/blob/main/chat-bots/src/Data/Chat/Server.hs#L91-L100)
to combine a chat bot with a server protocol.

I suspect there could be an interesting way of sequencing effects using
this concept. For example, a webserver as a `Mealy` machine annihilated
against a `Moore` machine representing the real world.
