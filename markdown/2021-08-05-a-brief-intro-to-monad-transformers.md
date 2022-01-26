---
author: Solomon Bothwell
title: A Brief Intro to Monad Transformers
---

A few friends have recently asked about literature introducing `Monad
Transformers`. The best introduction I have found was in [Haskell
Programming From First Principles](https://haskellbook.com/). If you
don\'t have, or want to purchase, this book, then here is a brief
explanation with examples.

Lets start with the problem they solve.

# Functors and Applicatives Compose

`Functor` and `Applicative` are both closed under composition. If we
compose any two `Functor` or `Applicative` types then we get a `Functor`
or `Applicative`.

We can do this composition with the `Compose` newtype:

``` haskell
newtype Compose f g a = Compose (f (g a))
```

The composition is witnessed by the `Functor` and `Applicative`
instances for `Compose`:

``` haskell
instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap :: (a -> b) -> Compose f g a -> Compose f g b
  fmap f (Compose x) = Compose (fmap (fmap f) x)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ pure $ pure a

  liftA2 :: (a -> b -> c) -> Compose f g a -> Compose f g b -> Compose f g c
  liftA2 f (Compose x) (Compose y) = Compose (liftA2 (liftA2 f) x y)
```

Now we can compose `Functors` together and get combined effects:

``` haskell
> xs = Compose [Just True, Just False, Nothing]
> :t xs
xs :: Compose [] Maybe Bool
> fmap not xs
Compose [Just False,Just True,Nothing]
```

``` haskell
> xs
Compose [Just True,Just False,Nothing]
> ys
Compose [Just False,Just True,Nothing]
> liftA2 (||) xs ys
Compose [Just True,Just True,Nothing,Just False,Just True,Nothing,Nothing,Nothing,Nothing]
```

We can do this with *any* two `Functors`. That is Really Cool:tm:.

But What about `Monad`? Can we write a `Monad` instance for `Compose`?

``` haskell
instance (Monad f, Monad g) => Monad (Compose f g) where
  return :: a -> Compose f g a
  return = pure

  (>>=) :: Compose f g a -> (a -> Compose f g b) -> Compose f g b
  Compose fga >>= f = _
```

What we are trying to do in this `>>=` instance is combine these two
functions:

``` haskell
Monad f => f a -> (a -> f b) -> f b
Monad g => g a -> (a -> g b) -> g b
```

Into this function:

``` haskell
(Monad f, Monad g) => f (g a) -> (a -> f (g b)) -> f g b
```

It turns out this is impossible. If we look at the combined `join` type
we can see the knot we have created:

``` haskell
join :: (Monad f, Monad g) => f (g (f (g a))) -> f (g a)
```

There simply is no way to implement this while keeping both `Monads`
polymorphic.

# Introducing Monad Transformers

The fundamental problem with composing two polymorphic `Monads` is the
polymorphism. If we could make one of the `Monads` concrete then we
could define `bind` and `join`:

``` haskell
  bind :: Monad f => f (Maybe a) -> (a -> f (Maybe b)) -> f (Maybe b)
  bind ma f =
    ma >>= \case
      Nothing -> pure Nothing
      Just a -> f a

join :: Monad f => f (Maybe (f (Maybe a))) -> f (Maybe a)
join ma = ma >>= \case
  Nothing -> pure Nothing
  Just ma' -> ma' >>= \case
    Just a -> pure $ Just a
    Nothing -> pure Nothing
```

This is `Monad Transformers` in a nutshell!

As long as we know at least one of the `Monads` at play then we have a
pathway to compose them. We can create special Transformer versions of
all our standard `Monads` which will have a slot for a second
polymorphic monad:

``` haskell
newtype IdentityT m a = IdentityT { runIdentityT :: m a }
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }
newtype ListT m a = ListT { runListT :: m [a] }
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
-- etc
```

An interesting side note is that you can recover all the standard
`Monad` variants by selecting `Identity` for the `m` parameter:

``` haskell
type Identity a = IdentityT Identity a
type Maybe a = MaybeT Identity a
type Either' e a = ExceptT e Identity a
type List a = ListT Identity a
type State s a = StateT s Identity a
-- etc
```

I\'ll show the instances for `MaybeT` and the rest are left as an
exercise for the reader:

``` haskell
instance Functor m => Functor (MaybeT m) where
  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f (MaybeT ma) = MaybeT $ fmap (fmap f) ma

instance Applicative m => Applicative (MaybeT m) where
  pure :: a -> MaybeT m a
  pure a = MaybeT $ pure $ pure a

  liftA2 :: (a -> b -> c) -> MaybeT m a -> MaybeT m b -> MaybeT m c
  liftA2 f (MaybeT ma) (MaybeT mb) = MaybeT $ liftA2 (liftA2 f) ma mb

instance Monad m => Monad (MaybeT m) where
  return :: a -> MaybeT m a
  return = pure

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  MaybeT ma >>= f =
    MaybeT $ ma >>= \case
      Nothing -> pure Nothing
      Just a -> runMaybeT $ f a
```

# Why is this useful?

With combined monadic effects we can do really nice sequencing and
combination of effects. For example..

Imagine we wanted to traverse an AST and replace the values with indices
referencing those values. Perhaps there is also the possiblity that
there are invalid values in your AST and if found we want to fail the
traversal. How might we write this code?

Our AST is just a binary tree and valid variables are given in a
`HashSet`:

``` haskell
data AST a = Leaf a | Node (AST a) (AST a)
  deriving (Show, Functor, Foldable, Traversable)

type VariableName = String
type Variables = S.HashSet VariableName
```

While traversing the tree we will need to keep a map of Variables to
Indices in state, generating new mappings as we encounter fresh
variables. For this we can use the `State` monad.

If we encounter an invalid variable then we need a way to exit early
from the traversal and report an error message. We can do this with the
`Either` Monad or its transformer variant `ExceptT`.

Our Transformer Stack will consist of `ExceptT` and `State`:

``` haskell
newtype AppM a = AppM { runAppM :: ExceptT String (State (M.Map VariableName Int)) a }
  deriving newtype (Functor, Applicative, Monad, MonadError String, MonadState (M.Map VariableName Int))
```

Now our program becomes a simple traversal where we can perform both
`State` effects and `Either` effects:

``` haskell
assignIndexToVariables :: AST VariableName -> Variables -> AppM (AST Int)
assignIndexToVariables ast variables = forM ast $ \var -> do
  unless (var `S.member` variables) $
    throwError $ "Unknown Variable " <> var
  cache <- get
  case M.lookup var cache of
    Just index -> pure index
    Nothing -> do
      let index = M.size cache
      put $ M.insert var index cache
      pure index

main :: IO ()
main =
  let vars = S.fromList ["a", "b", "c"]
      ast = Node (Leaf "a") (Node (Leaf "b") (Node (Leaf "a") (Leaf "c")))
  in print $ flip evalState mempty $ runExceptT $ runAppM $ assignIndexToVariables ast vars
```

``` haskell
> main
Right (Node (Leaf 0) (Node (Leaf 1) (Node (Leaf 0) (Leaf 2))))
```
