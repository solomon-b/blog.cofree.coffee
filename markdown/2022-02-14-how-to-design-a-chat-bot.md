---
author: Solomon Bothwell
date: 2022-02-14
title: How To Design A Chat Bot
---

I\'m working on a [library](https://github.com/cofree-coffee/cofree-bot)
for designing highly composable protocol agnostic chat bots. The design
is based on Mealy machines and heavily leverages Haskell\'s profunctor
machinery. I want to walk through the early stages of the design process
and how you might arrive at such an architecture.

## What is a Chat Bot?

Lets start by describing what we mean by a chat bot, then lets factor
out as much as possible until we arrive at a precise, elegant
abstraction.

A chat bot is some persistent application that reads and produces
messages over a messaging protocol. Bots can optionally hold internal
state which they update via incoming messages and which they use to
produce outgoing messages.

Bots can also optionally perform side effects out of band from the chat
protocol. For example, a bot might execute an HTTP request and then
return the result in band through the chat protocol.

So a chat bot holds state, sends and receives messages over a chat
protocol, and can potentially perform out of band effects.

Now lets try to describe these actions more formally. We know the bot
must receieve input to produce an output and, if it is stateful, produce
a new state. We can describe this with a record of functions:

``` haskell
data Bot state input output = Bot
  { receive :: input -> state -> state
  , respond :: input -> state -> output
  }
```

`receive` describes the act of updating the internal state from an input
and `respond` describes producing an output. We are still missing the
ability to perform out of band effects. We can describe this by
inscribing our outputs with an `m`:

``` haskell
data Bot m state input output = Bot
  { receive :: input -> state -> m state
  , respond :: input -> state -> m output
  }
```

At first blush this looks promising. We have the ability to update an
internal state, to emit responses, and to perform out of band effects.
However, does this fully describe the behavior of a bot?

We can update our state and we can produce output, but can we use our
updated state to produce the output? Sadly the answer is no.

Lets try again:

``` haskell
newtype Bot m s i o = Bot { runBot :: i -> s -> m (s, o) }
```

Now we have a single function which can update state and produce an
output *in a single operation*. This gives us what we want.

## Exploring Our Type

Now that we have our bot type, lets explore it a bit. We can see that it
*receives* an `i` and an `s` and it *produces* an `s` and an `o`. This
means that it is `Contravariant` over `i`, `Covariant` over `o`, and
`Invariant` over `s`. This tells us that our `Bot` is a `Functor`, a
`Profunctor`, and an `Invariant Functor`. If it were `Covariant` on both
`i` and `o` then it would be a `Bifunctor` rather then a `Profunctor`.

``` haskell
instance Functor m => Functor (Bot m s i) where
  fmap :: (o -> o') -> Bot m s i o -> Bot m s i o'
  fmap f (Bot bot) = Bot $ \i s -> fmap (fmap f) $ bot i s
```

``` haskell
instance Functor m => Profunctor (Bot m s) where
  dimap :: (i' -> i) -> (o -> o') -> Bot m s i o -> Bot m s i' o'
  dimap f g (Bot bot) = Bot $ \a -> fmap (fmap g) . bot (f a)
```

The order of type parameters doesn\'t allow the actual `Invariant`
typeclass, but we can define `invmap`:

``` haskell
invmap :: Functor m => (s -> s') -> (s' -> s) -> Bot m s i o -> Bot m s' i o
invmap f g (Bot b) = Bot $ \i s -> (b i (g s)) <&> bimap f id
```

Since `Bot` is a `Profunctor`, lets take look at some other related
structures:

``` haskell
class Profunctor p => Strong p where
  first' :: p a b  -> p (a, c) (b, c)
  second' :: p a b -> p (c, a) (c, b)

class Profunctor p => Choice p where
  left' :: p a b  -> p (Either a c) (Either b c)
  right' :: p a b -> p (Either c a) (Either c b)
```

`Strong` describes a `Profunctor` where you can use a product to
\'thread\' an additional parameter through the `Profunctor`. `Choice`
describes the same property with respect to co-products.

It turns out `Bot` satisifies both:

``` haskell
instance Functor m => Strong (Bot m s) where
  first' :: Bot m s i o -> Bot m s (i, c) (o, c)
  first' (Bot bot) = Bot $ \(a, c) -> fmap (fmap (, c)) . bot a

instance Applicative m => Choice (Bot m s) where
  left' :: Bot m s i o -> Bot m s (Either i x) (Either o x)
  left' (Bot bot) = Bot $ \i s ->
    case i of
    Left a -> fmap (fmap Left) $ bot a s
    Right c -> pure (s, Right c)
```

Another structure we might try is `Category`:

``` haskell
instance Monad m => Category (Bot m s) where
  id :: Bot m s i i
  id = Bot $ \i s -> pure (s, i)

  (.) :: Bot m s b c -> Bot m s a b -> Bot m s a c
  (.) (Bot bot1) (Bot bot2) = Bot $ \a s -> do
    (s', b) <- bot2 a s
    bot1 b s'
```

The fact that we have `Strong` and `Category` means we also have
`Arrow`:

``` haskell
instance Monad m => Arrow (Bot m s) where
  arr f = fmap f id
  first = first'
```

We will try to sort out the use of some of these structures later on.
For now, it is a great sign that our spec fits so many well defined
structures.

## Constructing Bots

Lets move on to building some bots. As we go along, we might discover
interesting uses for the structures defined previously.

We start with the simplest bot. Eg., one which receives and produces
`Text` and operates with no state or monadic effects:

``` haskell
simplestBot :: Bot Identity () Text Text
simplestBot = Bot $ \i s -> pure (s, "Hello, " <> i)
```

This bot will respond to all messages with a fixed response.

We can simplify the construction of other pure, stateless bots with a
new combinator:

``` haskell
pureStatelessBot :: Applicative m => (i -> o) -> Bot m s i o
pureStatelessBot f = Bot $ \i s -> pure (s, f i)
```

Given a `Monad` constraint on `m` (arising from our `Category`
instance), then `pureStatelessBot` is `arr` from `Arrow`:

``` haskell
pureStatelessBot' :: Monad m => (i -> o) -> Bot m s i o
pureStatelessBot' = arr
```

We can also construct effectful bots, such as one which performs random
number generation in `IO`:

``` haskell
coinFlipBot :: Bot IO () () Bool
coinFlipBot = Bot $ \_ s -> do
  gen <- newStdGen
  let (result, _) = random @Bool gen
  pure (s, result)
```

And of course, we could build a stateful bot:

``` haskell
todoBot :: Applicative m => Bot m [T.Text] T.Text T.Text
todoBot = Bot $ \i s ->
  case T.uncons i of
    Just ('>', todo) -> pure (todo:s, "Recorded todo!")
    Just ('<', _) | length s == 0 -> pure (s, "No more todos!")
    Just ('<', _) -> pure (tail s, head s)
    _ -> pure (s, "I didn't understand that.")
```

Notice that all of these bots *must* return a response regardless of the
input. This is something we will need to address shortly.

## Interpretation

Now that we have a few bots, we need some way to run them.

We can write a simple REPL-like bot interpreter. This will be a function
which receives a `Bot IO s Text Text` and produces a long lived `IO`
action that applies STDIN as input to the `Bot` and prints the `Bot`\'s
output to STDOUT.

``` haskell
runReplBot :: forall s. Bot IO s Text Text -> s -> IO ()
runReplBot bot = go
  where
    go :: s -> IO ()
    go state = do
  putStr "> "
  hFlush stdout
  input <- fmap T.pack $ getLine
  result <- try @SomeException $ runBot bot input state
  case result of
    Left _ -> go state
    Right (nextState, output) -> do
      putStrLn $ T.unpack output
      go nextState
```

Note: This interpreter will only work with `Bots` polymorphic on `m` or
where `m ~ IO`. A more general `replBot` would have the signature:
`forall m s. (MonadCatch m, MonadIO m) => Bot m s Text Text -> s -> m
()`.

We use `try` to capture exceptions as an `Either` value which we ignore
when recursing. This will make more sense later on.

Interpreters for arbitrary network protocols can be be written in the
same fashion. Choose appropriate input and output types for resolving
calls to your protocol of choice\'s API and then call out to your API
from an IO block.

We can use `runReplBot` to test out `simplestBot`:

``` bash
ghci> runReplBot simplestBot ()
> World
Hello, World
```

However, we still cannot run `coinFlipBot`. We require a `Bot IO s
Text Text` and `coinFlipBot` is `Bot IO s () Bool`.

To match it up with `runReplBot`, we need a way to map `Text -> ()` for
the input and `Bool -> Text` for the output. It turns out this is
precisely what `Profunctor` gives us!

``` haskell
coinFlipBot' :: Bot IO () Text Text
coinFlipBot' = dimap (const ()) (T.pack . show) coinFlipBot
```

One way to look at the behavior of `coinFlipBot'` is that it focuses on
a smaller input `()` inside of a larger structure `Text` and then embeds
a smaller output (`Bool`) inside a larger structure `Text`.

Another way to say that is we have *parsed* out of `Text` to pick a `()`
and *pretty printed* into `Text` to embed a `Bool`.

Our work identifying algebraic structures is already paying off.

## Conditional Responses

Now we have defined a few simple bots and demonstrated how to interpret
them in a REPL-like environment. We still have an unsolved problem,
these bots are rather talkative. They must responsd to *all* input they
receieve. We need to sort out a way for bots to conditionally produce
output.

Our first thought might be to change our `Bot` type to either of:

``` haskell
newtype Bot m s i o = Bot { runBot :: i -> s -> m (Maybe (s, o)) }
newtype Bot m s i o = Bot { runBot :: i -> s -> m [(s, o)] }
```

However, both of those can break some desirable composition behavior.
Another option could be `ListT` from `MTL`, but it has some
[problems](https://wiki.haskell.org/ListT_done_right). The correct
solution would be to use a Streaming library--which is what we do in
[the library](https://github.com/cofree-coffee/cofree-bot) that inspired
this blog post. The solution we have chosen for expediance here is to
leverage `Alternative`.

With `IO`\'s `Alternative`, we can use `empty` to throw an exception
which we catch in our interpreter. The exception handling is already
included in `runReplBot`. Bots which don\'t specify a concrete Monad
will get interpreted into `IO` and throw an exception when called from
`runReplBot`.

Lets see how this would work with `coinFlipBot`:

``` haskell
coinFlipBot' :: Bot IO () Text Text
coinFlipBot' = Bot $ \i s ->
  if i == "flip a coin"
    then fmap (fmap (T.pack . show)) $ (runBot coinFlipBot) () s 
    else empty
```

We can no longer use `dimap` because our *focus* operation is not pure
due to our use of `empty`.

We can, however, define a new combinator `lmapMaybe` to generalize over
the optionality we just introduced and peel it out of `coinFlipBot'`:

``` haskell
lmapMaybe :: Alternative m => (i' -> Maybe i) -> Bot m s i o -> Bot m s i' o
lmapMaybe f (Bot bot) = Bot $ \i' s ->
  case f i' of
    Nothing -> empty
    Just i -> bot i s

coinFlipBot' :: Bot IO () Text Text
coinFlipBot' = lmapMaybe parse $ fmap prettyPrint coinFlipBot
  where
    parse i = if i == "flip a coin" then Just () else Nothing
    prettyPrint = (T.pack . show)
```

What we are seeing in `coinFlipBot'` is contravariant and covariant
mappings of our input and output to *focus* and *embed* structures
respectively. In the contravariant case we are using a special variation
of `lmap` which leverages `Alternative` to produce optional outputs.

## Composition

Our goal now is to take two bots and \'laterally\' compose them together
to combine their behaviors. At the type level, what this looks like is
combining each of the three type parameters of our `Bots` with some
binary associative type constructors:

``` haskell
_ :: Bot m s i o -> Bot m s' i' o' -> Bot m (t1 s s') (t2 i i') (t3 o o')
```

For example, we could use `(,)` in all three positions:

``` haskell
_ :: Bot m s i o -> Bot m s' i' o' -> Bot m (s, s') (i, i') (o, o')
```

This would give us a single bot which given a combined input `(i, i')`
will perform the behaviors of both our original bots and give a combined
output `(o, o')`.

What we want is a way to conditionally run *either* of the two bots
based on the input we receive. This indicates that we want to use
`Either` for `i` and `o`. However, we don\'t want to use `Either` for
our state `s`. Instead we should use `(,)` to ensure that regardless of
which bot we choose to execute, we have it\'s required state available.

We call this combinator `\/`:

``` haskell
infixr \/
(\/) :: Bot m s i o -> Bot m s' i' o' -> Bot m (s, s') (Either i i') (Either o o')
```

As one might expect from a \'lateral composition\' operator, it is
associative up to reshufflings of the binary type constructors. `\/` (in
uncurried form) is described by the `Semigroupal` typeclass from the
[monoidal-functors](https://hackage.haskell.org/package/monoidal-functors-0.1.1.0/docs/Data-Trifunctor-Monoidal.html#v:combine)
library.

``` haskell
-- Data.Functor.Monoidal
class (Associative t1 cat, Associative t0 cat) => Semigroupal cat t1 t0 f where
  combine :: (f x `t0` f x') `cat` f (x `t1` x') 

-- Data.Bifunctor.Monoidal
class (Associative t1 cat, Associative t2 cat, Associative to cat) => Semigroupal cat t1 t2 to f where
  combine :: cat (to (f x y) (f x' y')) (f (t1 x x') (t2 y y')) 

-- Data.Trifunctor.Monoidal
class (Associative t1 cat, Associative t2 cat, Associative t3 cat, Associative to cat) => Semigroupal cat t1 t2 t3 to f where
  combine :: to (f x y z) (f x' y' z') `cat` f (t1 x x') (t2 y y') (t3 z z') 
```

We have 3 type constructors we wish to monoidally combine (`s`, `i`, and
`o`) so we choose the `Data.Trifunctor.Monoidal.Semigroupal` class:

``` haskell
instance Functor m => Semigroupal (->) (,) Either Either (,) (Bot m) where
  combine :: (Bot m s i o, Bot m s' i' o') -> Bot m (s, s') (Either i i') (Either o o')
  combine (Bot bot, Bot bot') = Bot $ \ei (s, s') ->
    case ei of
    Left i -> fmap (bimap (,s') Left) $ bot i s
    Right i' -> fmap (bimap (s,) Right) $ bot' i' s'

infixr \/
(\/) :: Functor m => Bot m s i o -> Bot m s' i' o' -> Bot m (s, s') (Either i i') (Either o o')
(\/) = curry combine
```

Now we can use `\/` to compose a few bots:

``` haskell
coinFlipBot :: Bot IO () () Bool
coinFlipBot = Bot $ \_ s -> do
  result <- randomIO
  pure (s, result)

diceRollBot :: Bot IO () () Int
diceRollBot = Bot $ \i s -> do
  result <- randomRIO (1, 6)
  pure (s, result)

sumBot :: Bot IO ((), ()) (Either () ()) (Either Int Bool)
sumBot = diceRollBot \/ coinFlipBot
```

`sumBot` will execute a dice roll if it receives a `Left ()` or a coin
flip if it receives a `Right ()`. We can then use `lmapMaybe` and a few
other tools to produce an approprate parser and pretty printer:

``` haskell
sumBot' :: Bot IO ((), ()) Text Text
sumBot' = (lmapMaybe parse) $ fmap prettyPrint sumBot
  where
    parse :: Text -> Maybe (Either () ())
    parse "roll a die" = pure $ Left ()
    parse "flip a coin" = pure $ Right ()
    parse _ = empty

    prettyPrint :: Either Int Bool -> Text
    prettyPrint = indistinct . bimap (T.pack . show) (T.pack .show)

    indistinct :: Either a a -> a
    indistinct = either id id
```

``` bash
ghci> runReplBot sumBot' ((), ())
> flip a coin
True
> roll a die
4
> x
> 
```

## Transformations

At this point we can build bot behaviors around arbitrary inputs and
outputs, combine behaviors to produce composite bots, and interpret them
in arbitrary protocols. Lets explore a few other interesting ways of
transforming a `Bot`.

If we look at the kind of `Bot` we see:

``` bash
type KBot = (Type -> Type) -> Type -> Type -> Type -> Type
```

Now, imagine something with kind `KBot -> KBot`. This would represent
something that recieves a `Bot` and produces some other `Bot`. This is
an overally powerful kind signature and allows for *any* transformation
on a bot. For this reason its not very descriptive, but it gives an
intuition for what it means to transform a bot.

For a first example, imagine we want to take one of our bots, such as
`coinFlipBot`, and run it on some protocol with distinct chat rooms. We
want our `coinFlipBot` to be able to receive messages annotated with
their source room and then produce messages annotated with the target
room.

We can describe this with a type alias that annotates a bot\'s input and
output with \'room awareness\':

``` haskell
type RoomAware bot m s i o = bot m s (RoomID, i) (RoomID, o)
```

Now we need a function to inhabit this type. We are looking for
something that descibes the act of threading a type through our `Bot`
via the product structure `(,)`.

It just so happens that we already have that! This is precisely the
behavior of the `Strong` typeclass we implemented earlier:

``` haskell
class Profunctor p => Strong p where
  first' :: p a b  -> p (a, c) (b, c)
  second' :: p a b -> p (c, a) (c, b)
```

This means we can make our `coinFlipBot` room aware through the
appliction of `second'`:

``` haskell
roomAwareBot :: RoomAware Bot IO () () Bool
roomAwareBot = second' coinFlipBot
```

Another interesting bot transformation is adding session state. Earlier
we defined a `todoBot` which allowed a user to construct a todo list. We
might want to allow multiple users to store their own todo lists. We
could redesign the `todoBot` to support this explicitly, but we want to
be able to define precise bots with narrow scopes which we can then
extend through composition.

What we really want is a way \'sessionize\' a bot. This will involve
transforming the bot\'s `s` state parameter in addition to its input and
output. This is still a rough sketch of an idea and I hope to write a
follow up post going into greater detail, but the the core idea is to
define the following types:

``` haskell
newtype SessionState s = SessionState { sessions :: Map.Map Int s }
  deriving (Show, Semigroup, Monoid)

data SessionInput i =
    InteractWithSession Int i
  | StartSession
  | EndSession Int

data SessionOutput o =
    SessionOutput Int o
  | SessionStarted Int
  | SessionEnded Int
  | InvalidSession Int

type Sessionized bot m s i o = Bot m (SessionState s) (SessionInput i) (SessionOutput o)
```

These types describe a language for interacting with a sessionized bot.
Now we need a function for sessionizing bots:

``` haskell

sessionize
  :: Monad m
  => s
  -> Bot m s i o
  -> Sessionized m s i o
sessionize = _
```

A \'sessionized\' bot would receive `SessionInput` input and dispatch
the wrapped `i` term along with the appropriate state `s` term to the
embedded bot. This idea isn\'t fully developed, but I hope it gives you
an idea of what kinds of transformations are possible with this
architecture.

## Conclusion

We have demonstrated the core bot architecture as well as constructing,
interpreting, composing, and extending bots in various dimensions. More
so then explaining how to build a chat bot, I hope this post inspires
you to think more algebraically about your program architectures and to
leverage more of the powerful abstractions available to us with Haskell.

Special thanks to [\@masaeedu](https://github.com/masaeedu),
[\@iris](https://github.com/conjunctive), and everyone else in the
[Cofree-Coffee Org](https://github.com/cofree-coffee/).
