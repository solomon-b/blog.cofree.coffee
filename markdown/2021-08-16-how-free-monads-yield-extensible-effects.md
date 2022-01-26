---
author: Solomon Bothwell
title: How Free Monads Yield Extensible Effects
---

The `Free` monad gives you a `Monad` for *any* `Functor`. The `Free`
monad can also be used to construct extensible effect systems. I never
understood why `Free` why this was the case. It turns out it is deeply
connected to their ability to yield monads for functors.

# The Free Type

For a warmup, lets review `Free`[^1]:

``` haskell
data Free f a where
  Pure :: a -> Free f a
  Free :: f (Free f a) -> Free f a
```

`Free` allows you to build up an AST which describes monadic actions for
any given `Functor` and defers the interpretation of that AST to a later
date. In other words, `Free` breaks apart the *syntax* and *semantics*
of your monadic effects and lets you describe effects syntacally without
assigning them any sort of semantics.

How does that work?

Looking at `Free`\'s constructors they bear a striking resemblence to
`pure` and `join`:

``` haskell
pure :: Applicative f => a ->      f a
Pure ::                  a -> Free f a

join :: Monad m => m (     m a) ->      m a
Free ::            f (Free f a) -> Free f a
```

Squint your eyes and ignore the `Free` type constructors and you can see
the symmetry here. The chief difference is that `Free`\'s data
constructors lack `Applicative` and `Monad` constraints. This is because
it doesn\'t actually perform any effects, it is merely a syntax tree
describing effects yet to be interpreted.

Now lets take a closer look `Free`\'s Typeclass instances.

# Free is a Monad

``` haskell
instance Functor f => Functor (Free f) where
  fmap :: (a -> b) -> Free f a -> Free f b
  fmap f (Pure a) = Pure (f a)
  fmap f (Free m) = Free $ fmap (fmap f) m

instance Functor f => Applicative (Free f) where
  pure :: a -> Free f a
  pure = Pure

  liftA2 :: (a -> b -> c) -> Free f a -> Free f b -> Free f c
  liftA2 f (Pure a) m = fmap (f a) m
  liftA2 f (Free a) m = Free $ fmap (flip (liftA2 f) m) a

instance Functor f => Monad (Free f) where
  return :: a -> Free f a
  return = Pure

  (>>=) :: Free f a -> (a -> Free f b) -> Free f b
  (>>=) (Pure a) f = f a
  (>>=) (Free a) f = Free $ fmap (>>= f) a
```

There are two things to notice here:

1.  For all instances `f` need only be a `Functor`.

2.  These instances are utterly boring and essentially just serve to
    thread `f`\'s `fmap` throughout the syntactic structure of the
    `Free` data constructors.

Both of these points bring home the fact that `Free` really only deals
with syntax. If we want a semantics for our monadic structure then we
must create one via an interpreter.

For example, in Oleg\'s Paper[^2] he gives an example of modeling the
`State` monad in `Free`:

``` haskell
eta :: Functor f => f a -> Free f a
eta = Free . fmap Pure

type FState s = Free (State s)

getF :: FState s s
getF = eta get

putF :: s -> FState s ()
putF = eta . put

runStateF :: FState s a -> s -> (a, s)
runStateF (Pure x) s = (x, s)
runStateF (Free m) s =
  let (m', s') = runState m s in runStateF m' s'
```

Here we have chosen `State` as our functor and we use `eta` to lift
state operations into `Free`. We use those lifted operations to create a
syntax tree describing our stateful program.

We then implement the *semantics* of our `State` effect via the
interpreter `runStateF`. The intepreter recurses through our `Free` AST
and interprets the `Free` data constructors as the operations of our
`State` effect. Thus we have recreated the monadic operations of the
`State` monad via the `State` `Functor` and `Free`\'s data constructors.

# Extensible Effects

Armed with a reasonable understanding of `Free` we can approach the main
topic of this blog post. How does seperating the syntax and semantics of
monadic effects help us with composing our effects?

Well, it is actually quite simple! Monads do not compose, but functors
do compose. `Free` allows us to construct a `Monad` for any `Functor`.
Therefore, if we can somehow compose our functors then we can use `Free`
to produce a `Monad` with the composed effects of the two functors.

The `Compose` newtype gives us a `Functor` made up of right-to-left
composition of our functors, but this won\'t do what we want. We don\'t
want to combine our effects functorily, rather we want access to both
`f a` and `g a` within a single monadic context.

In other words, we want the `Sum` of two functors:

``` haskell
data Sum f g a = InL (f a) | InR (g a)
  deriving Functor
```

We need `Sum` rather then `Either` so that both nested functors use the
same `a` parameter. With `Either` we would only have a `Functor` over
the `Right` term.

# The Simplest Effects System

With `Sum` we can create the world\'s simplest effects system. In this
system we will be able to pick two `Functors` patch them into `Free` and
then write an interpreter to compose their effects.

Our Effect `Monad` will look like:

``` haskell
type SimplestFX f g = Free (Sum f g)
```

For a first attempt we will hardcode our interpreter for `State` and
`Either`:

``` haskell
runFX :: s -> SimplestFX (State s) (Either e) a -> Either e (a, s)
runFX s (Pure a) = Right (a, s)
runFX s (Free (InL m)) = let (m', s') = runState m s in runFX s' m'
runFX s (Free (InR (Left e))) = throwError e
runFX s (Free (InR (Right m))) = runFX s m
```

All `State` operations are in left branch of our `Sum` and all `Either`
operations are in the right branch. This allows our interpreter to know
exactly what effect to perform as we traverse the AST.

We lift our effects using `eta . InL` and `eta . InR` to lift into the
left and right branches of the `Sum` respectively.

Now we can rewrite the Tree Traversal example from my [prevous
post](https://blog.cofree.coffee/2021-08-05-a-brief-intro-to-monad-transformers/)
on Monad Transformers:

``` haskell
type VariableName = String
type Variables = S.HashSet VariableName

data AST a = Leaf a | Node (AST a) (AST a)
  deriving (Show, Functor, Foldable, Traversable)

assignIndexToVariables :: AST VariableName -> Variables -> SimplestFX (State (M.Map VariableName Int)) (Either String) (AST Int)
assignIndexToVariables ast variables = forM ast $ \var -> do
  unless (var `S.member` variables) $
    eta $ InR $ throwError $ "Unknown Variable " <> var
  cache <- eta $ InL get
  case M.lookup var cache of
    Just index -> pure index
    Nothing -> do
      let index = M.size cache
      eta $ InL $ put $ M.insert var index cache
      pure index

main :: IO ()
main =
  let vars = S.fromList ["a", "b", "c"]
      ast = Node (Leaf "a") (Node (Leaf "b") (Node (Leaf "a") (Leaf "c")))
  in print $ runFX mempty $ assignIndexToVariables ast vars
```

# Generalizing

In our last example, the interpreter consists of structural recursion on
`Free` along with explicit interpretations of our effects into some hard
coded result type `Either e (a, s)`. We can break up the recursion and
interpretation to give us a more general API:

``` haskell
runFX' :: Monad m => (forall x. f x -> m x) -> (forall x. g x -> m x) -> SimplestFX f g a -> m a
runFX' _ _ (Pure a) = pure a
runFX' interF interG (Free (InL f)) = let m = interF f in m >>= runFX' interF interG
runFX' interF interG (Free (InR g)) = let m = interG g in m >>= runFX' interF interG
```

Now we write an interpreter into some concrete `Monad` with the
semantics we desire:

``` haskell
runApp :: Monoid s => SimplestFX (State s) (Either e) a -> ExceptT e (State s) a
runApp = runFX' lift (ExceptT . pure)
```

And finally we run our effects using the transformer stack we
interpreted our program into:

``` haskell
main :: IO ()
main =
  let vars = S.fromList ["a", "b", "c"]
      ast = Node (Leaf "a") (Node (Leaf "b") (Node (Leaf "a") (Leaf "c")))
  in print $ flip evalState mempty $ runExceptT $ runApp $ assignIndexToVariables ast vars
```

In this case we are using `ExceptT e (State s) a` but we can choose any
semantic context we desire. This reveals another super power of
Extensible Effects.

We can use the exact same syntactic construction, eg. code, and have
multiple semantic interpretations. For example, we could have a program
that performs some calculation and then dispatches the calculation
result to some store. We would be able to swap out store interpretations
between writing to a file on disk, writing to a database, and sending
out an HTTP request, etc.

# Further Generalizations

At this point our effect system can only handle two effects and they
must have `Functor` instances.

We can replace `Sum` with an open union to be able to include an
arbitrary number of effects in our program. Haskell does not support
open unions natively but we can use some type level tricks to support
them.

We can also use `Coyoneda` to construct the `Freer Monad` which removes
the requirement of having a `Functor` instance for our types! I\'ll
cover all of this in a later blog post.

[^1]: [Free and Freer Monads: Putting Monads Back into
    Closet](http://okmij.org/ftp/Computation/free-monad.html)

[^2]: [Free and Freer Monads: Putting Monads Back into
    Closet](http://okmij.org/ftp/Computation/free-monad.html)
