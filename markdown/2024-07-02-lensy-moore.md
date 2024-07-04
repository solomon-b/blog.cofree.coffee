---
author: Solomon Bothwell
title: Lensy Moore
---

How far can we get leveraging the `lens` library in Haskell to model
Moore Machines and Wiring Diagrams?

## Morphisms in Poly

First off we need a rapid pre-amble on the category of polynomials.

Objects in `Poly` are defined as sums of representable functors and can
be written in Agda as:

``` Agda
record Poly : Set where
  no-eta-equality
  field
    Base : Set
    Fiber : Base → Set
```

Since our goal is an implementation in Haskell we are going to ignore
the dependency in `Fiber` and just look at non-dependent monomials which
have the following shape:

``` Agda
-- | S × Yᵀ
_y^_ : Set → Set → Poly
(_y^_ S T) .Base = S
(_y^_ S T) .Fiber  = λ _ → T
```

For example, `2y^2` is defined as `Fin 2 y^ Fin 2` which would expand
out to:

``` Agda
2y^2 : Poly
Base 2y^2 = Fin 2
Fiber 2y^2 = λ where
  zero → Fin 2
  suc zero → Fin 2
```

Morphisms (poly maps) are then defined as:

``` Agda
record _⇒_ (P Q : Poly) : Set where
  no-eta-equality
  field
    map-base : P .Base → Q .Base 
    map-fiber : (base : P .Base ) → Q .Fiber (map-base base) → P .Fiber tag
```

Here we map from the Base of `P` to the Base of `Q` but then we do a
weird backwards feeling contravariant move and say \'given a particular
base of `Base
P` we have a map from the `Fiber Q (map-base base)` to the
`Fiber P base`.

### Lenses in Poly

If you squint your eyes at the definition of poly maps you can see that
`map-base` and `map-fiber` have the same shape as `get` and `set` for
lenses. This is because poly maps turn out to be (dependent) lenses:

``` agda
   map-base : P .Base → Q .Base 
-- get      : S       → A 
   map-fiber : (tag : P .Base ) → Q .Fiber (map-base tag) → P .Fiber tag
-- set       : (s : S)          → B (get s)               → T
```

Since were ultimately going to translate into Haskell we can ignore the
dependency in the `Fiber` map and just look at how to to build
non-dependent lenses as poly maps:

``` Agda
Lens : Set → Set → Set → Set → Set
Lens S T A B = S y^ T ⇒ A y^ B

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

### Moore Machines in Poly

Poly maps of the form `Syˢ ⇒ Oyᴵ` are Moore Machines:

``` agda
   map-base : P .Base → Q .Base 
-- observe  : S       → O
   map-fiber  : (tag : P .Base ) → Q .Fiber (map-base tag) → P .Fiber tag
-- transition : S                → I                       → S
```

``` Agda
Moore : Set → Set → Set → Set
Moore S I O = S y^ S ⇒ O y^ I

moore : ∀{S I O : Set} → (S → O) → (S → I → S) → Moore S I O
moore output transition .map-base = output
moore output transition .map-fiber s = transition s
```

### Wiring Diagrams in Poly

[Wiring
diagrams](https://cgi.cse.unsw.edu.au/~eptcs/paper.cgi?ACT2020:32.pdf)
(the combinatorial data defining a string diagram) are poly maps `P ⇒ Q`
where `Q` is the outer interface of the diagram and `P` describes the
interior mappings of the diagram.

``` Agda
WiringDiagram : Poly → Poly → Set
WiringDiagram P Q = P ⇒ Q
```

``` example
  ┌───────────────────────┐
  │  ┌───┐   ┌───┐        │
──┼──┤   ├───┤   ├────────┼──
  │  │   ├┐  └───┘        │
  │  └───┘│  ┌───┐        │
  │       └──┤   ├┐       │
  │          └───┘│ ┌───┐ │
  │               └─┤   │ │  
──┼─────────────────┤   ├─┼──
  │                 └───┘ │
  └───────────────────────┘
```

In this example, `P` would describe the collection interior boxes, `Q`
would describe the exterior interface of the diagram, and the poly map
`P ⇒ Q` describes how to wire all of these components together.

``` agda
-- The base of 'P' is the output wires of each internal slot.
-- The fiber of 'P' is the input wires of each internal slot.
P : {Set} → Poly
P {A} = (A × A × A × A × A) y^ (A × A × A × A × A)

-- The base of 'Q' is the output wires of the diagram.
-- The fiber of 'Q' is the input wires of the diagram.
Q : {Set} → Poly
Q {A} = (A × A) y^ (A × A)

-- Here we label input/output wires for P/Q from top to bottom left to right:
P⇒Q : {A : Set} → P ⇒ Q
-- The base-map declares how to wire the inner box outputs to the outer box outputs:
map-base P⇒Q (pout1 , pout2 , pout3 , pout4 , pout5) = pout3 , pout5
-- The fiber-map declares how to wire the outer box inputs and the inner box outputs to the inner box inputs. 
map-fiber P⇒Q (pout1 , pout2 , pout3 , pout4 , pout5) (qin1 , qin2) = qin1 , pout1 , pout2 , pout4 , qin2
```

NOTE: For convenience in this example, if we assume all wires carry the
same arbitrary type `A`.

### Composition in Poly

Since wiring diagrams are poly maps, we can use wiring diagrams to
visualize how composition works in `Poly`.

Given poly maps `g : P ⇒ Q` and `f : Q ⇒ R`:

``` example
       ┌─────────┐
       │  ┌───┐  │ 
g :  ──┼──┤  P├──┼───
       │  └───┘ Q│
       └─────────┘

       ┌─────────┐
       │  ┌───┐  │ 
f :  ──┼──┤  Q├──┼───
       │  └───┘ R│
       └─────────┘
```

Composition is defined as:

``` example
            ┌───────────────┐
            │  ┌─────────┐  │
            │  │  ┌───┐  │  │ 
compose : ──┼──┼──┤  P├──┼──┼──
            │  │  └───┘ Q│  │
            │  └─────────┘ R│
            └───────────────┘
```

Sticking with the wiring diagram metaphor, we are \'plugging\' `g` into
the open \'slot\' of `f` creating a new poly map `P ⇒ R` which maps you
from interface `Q` to the interface `R`, giving you a new way to
interact with `P`.

In this sense `Poly` is the language of interface design.

To make this a little more concrete, imagine `g` were a Moore Machine
`Syˢ ⇒
Oyᴵ` and `f` were some poly map `Oyᴵ ⇒ Byᴬ`. By composing them together
we create a new poly map `Syˢ ⇒ Byᴬ` where `f` maps `A` inputs to `I`
inputs and `B` outputs to `O` outputs which are fed into the original
Moore Machine.

## Lensy Moore {#lensy-moore-1}

With that rapid fire overview of `Poly` concepts out of the way we can
move on to our `lens` based encodings. To make this work we need to
overload `view` and `set` to get some more polymorphism:

``` haskell
view :: Lens s t a b -> s -> a
view l s = getConst $ l Const s

set :: Lens s t a b -> s -> b -> t
set l s b = runIdentity $ l (\_ -> Identity b) s
```

Now we can define `Moore` as a type alias and use `view` and `set` for
our observation and transition functions:

``` haskell
-- | Syˢ ⇒ Oyᴵ
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

`Poly` has infinite monoidal structures, and five notable ones. Of those
five is the parallel product aka `tensor` aka `_⊗_` which is
particularly useful when working with wiring diagrams.

``` Agda
infixr 7 _⊗_
_⊗_ : Poly → Poly → Poly
(P ⊗ Q) .Base  = Base P × Base Q
(P ⊗ Q) .Fiber (ptag , qtag) = Fiber P ptag × Fiber Q qtag
```

`tensor` takes the product of both the base and the fiber of polynomials
`P` and `Q`.

This translates quite nicely to `Moore`:

``` haskell
tensor :: Moore s i o -> Moore t i' o' -> Moore (s, t) (i, i') (o, o')
tensor m n =
  let observe' (s, t) = (observe m s, observe n t)
      transition' (s, t) (a, a') = (transition m s a, transition n t a')
  in lens observe' transition'
```

`tensor` also happens to be the `combine` operation of a 3 parameter
monoidal functor `(C, (,), ()) → (D, (,), ())`:

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

With `tensor` we can take two `Moore` and run them in parallel as a
single `Moore`.

## Fibonacci

As our big example, we can create a fibonacci wiring diagram and use it
to wire up the requisite `Moore` machines to build a Fibonacci
algorithm.

Our wiring diagram looks like this:

        ┌────────────────────────┐
        │  ┌───────┐             │ 
        │  │┌─────┐│  ┌─────┐    │ 
        │  └┤ℤ    ││  │     │    │ 
      ──┤   │  P ℤ├┴──┤ℤ Q ℤ├┬───┼──
    Unit│  ┌┤ℤ    │   │     ││   │ℤ
        │  │└─────┘   └─────┘│   │ 
        │  └─────────────────┘   │ 
        └────────────────────────┘

As we saw earlier, we can mechanically translate from this Wiring
Diagram to a poly map
`fibWiring = (ℤ × ℤ) y^ ((ℤ × ℤ) × ℤ) ⇒ ℤ y^ Unit`.

At this point we have a lot of metaphors floating around trying to
explain what poly map actually means. To put it most simply, we have two
functions which we can derive by plugging in the types from `fibWiring`
into our definition of a poly map:

``` haskell
map-base : P .Base → Q .Base 
map-base : (ℤ × ℤ) → ℤ

map-fiber : (tag : P .Base) → Q .Fiber (map-base tag) → P .Fiber tag
map-fiber : (ℤ × ℤ)         → Unit                    → ((ℤ, ℤ), ℤ)
```

Under the Wiring Diagram perspective, these functions represent a plan
for how to wire the \'inner\' and \'outer\' polynomials together. Here
the inner one is `P ⊗
Q`, eg., `(ℤ × ℤ) y^ ((ℤ × ℤ) × ℤ)`, and the outer one is `ℤ y^ Unit`.

The `Fiber` represents \'inputs\' to the polynomial and the `Base`
represents \'outputs\'.

`map-base` is a function that wires the outputs from the inner
polynomial to the outputs of the outer polynomial.

`map-fiber` is a function that wires the output of the inner polynomial
and the input of the \'outer\' polynomial to the \'inputs\' of the inner
polynomial.

In the particular case of `fibWiring` the outer polynomial (aka the
public interface) recieves a `Unit` value and produces an integer.

We drop the `Unit` value and wire the output from `Q` to the output of
the public interface. We also wire the outputs of both `P` and `Q` into
the inputs of `P` and the output of `P` into `Q`.

``` haskell
fibWiring :: Lens (Int, Int) ((Int, Int), Int) Int ()
fibWiring = 
  lens
    -- The Q output is wired to the outer interface's output:
    (\(pout, qout) -> qout)
    -- The P and Q outputs are wired into the P input and
    -- the P output is wired into the Q input:
    (\(pout, qout) () -> ((pout, qout), pout))
```

These diagram describes the \'schema\' of an algorithm. It just
describes how to pass values around along wires.. To build an actual
algorithm we need to plug `Moore` machines into those empty boxes to do
computation on the propagated values.

We do this by picking appropriate `Moore` machine(s) which when tensored
together have input and output types which match the inner polynomial of
`fibWiring`.

In our case we want `P` to sum its input integers and `Q` to act as a
delay line emitting the previous summed value. Picking `plus` for `P`
and `delay` for `Q` gives us exactly that.

``` haskell
-- | (ℤ × ℤ) y^ (ℤ × ℤ) ⇒ (ℤ × ℤ) y^ ((ℤ × ℤ) × ℤ)
plusDelay :: Moore (Int, Int) ((Int, Int), Int) (Int, Int)
plusDelay = plus `tensor` delay
```

Now we have two poly maps encoded as lenses one representing two Moore
machines running in parallel and the other representing how to wire
those Moore Machines together with a new public interface.

All we need to do is compose them together to create our final Fibonacci
Moore machine:

``` haskell
-- | (ℤ × ℤ) y^ (ℤ × ℤ) ⇒ ℤ y^ Unit
fib :: Moore (Int, Int) () Int
fib = plusDelay . fibWiring
```

If we run this thing we get just what we expected:

    > runMoore fib (1, 0) [(), (), (), (), (), (), (), (), (), ()]
    [0,1,1,2,3,5,8,13,21,34]

Trippy!

NOTE: I removed the final section on interactions between `Mealy` and
`Moore` as it needs a bit more work and will be included in a later
post.
