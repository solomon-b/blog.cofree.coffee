---
author: Solomon Bothwell
title: Servant Auth Roles
---

I wanted a nice way to create a roles system on top of `servant-auth`.
This post walks through the design and implementation process. The end
result is quite similar to OCharle\'s [Who Authorized These
Ghosts](https://blog.ocharles.org.uk/blog/posts/2019-08-09-who-authorized-these-ghosts.html).
You can see the final result
[here](https://github.com/solomon-b/servant-auth-roles).

The immediate thing I wanted was auth roles/permission sets in my
Servant API types such that I can define distinct handlers for each auth
role. I also want to maintain compatibility with `AuthProtect`.

# The idea

My first move was to sketch out an imaginary interface for the library.
My hope is that starting from the interface that the code would reveal
its implementation to me.

I wanted to define my roles as a sum type and then instantiate a
typeclass to describe the role check conditions. This way I could design
a variety of role systems (hierarchical, non-hierarchical, set based,
etc).

``` haskell
data UserRole = Viewer | Editor | Admin
  deriving (Eq, Ord, Show)

instance CheckRole 'Viewer where
  checkRole role = role >= Viewer

instance CheckRole 'Editor where
  checkRole role = role >= Editor

instance CheckRole 'Admin where
  checkRole role = role >= Admin
```

`CheckRole` allows you to use arbitrary predicates. In this case I use
Ord, because I want fall-through but we could just as easily use Eq, or
even set membership if you want to model something more like
\"permissions\" then \"roles.\"

Then we would define our typical Servant auth type and `AuthServerData`
instance:

``` haskell
data Authz = Authz { userRole :: UserRole, userName :: String }
  deriving (Show)

type instance AuthServerData (AuthProtect "test-auth") = Authz
```

And finally some magical Servant combinator `RequireRole` I can use in
my API types to assign auth roles to routes. The combinator would
introduce a role permission check before firing the handler. If the
`UserRole` value from `Authz` doesn\'t satisfy the `RoleChecK` for a
handler it fails and tries the next matching route.

``` haskell
type PanelAdminAPI =
  RequireRole "test-auth" 'Admin
    :> "panel"
    :> Get '[JSON] String

type PanelEditorAPI =
  RequireRole "test-auth" 'Editor
    :> "panel"
    :> Get '[JSON] String

type PanelViewerAPI =
  RequireRole "test-auth" 'Viewer
    :> "panel"
    :> Get '[JSON] String

type API = PanelAdminAPI :<|> PanelEditorAPI :<|> PanelViewerAPI

server :: Server API
server = panelAdmin :<|> panelEditor :<|> panelViewer
  where
    panelAdmin :: Authz -> Handler String
    panelAdmin _ = pure "admin panel"

    panelEditor :: Authz -> Handler String
    panelEditor _ = pure "editor panel"

    panelViewer :: Authz -> Handler String
    panelViewer _ = pure "viewer panel"
```

This obviously doens\'t work as is, but I wanted a DX roughly like this.

# Making the idea work

The key to this idea is going to be the `RequireRole` `HasServer`
instance.

## HasServer in a nutshell

But before that, allow me to briefly explain the `HasServer` class. This
isn\'t a Servant tutorial so I\'m going to skip a lot of details.

It has one associated type and two methods:

``` haskell
class HasServer api context where
  type ServerT api (m :: Type -> Type) :: Type

  route :: Proxy api -> Context context -> Delayed env (Server api) -> Router env

  hoistServerWithContext
    :: Proxy api -> Proxy context -> (forall x. m x -> n x) -> ServerT api m -> ServerT api n

type Server api = ServerT api Handler
```

`api` is our Servant Combinator. It describes a part of an API\'s
structure. The `HasServer` instance is sort of a bridge between that API
description and an actual route handler that serves it.

The associated type `ServerT` tells the compiler how to translate from
the `api` type to a fragment of your handler\'s type signature.

For example the `ServerT` associated type for `Verb` is:

``` haskell
type ServerT (Verb method status ctypes a) m = m a
```

This means `Verb 'GET 200 '[JSON] a` maps to `m a` in your handler\'s
signature.

`route` builds a dispatch tree called a `Router` that Servant then
converts via `serve` into a WAI Application for processing requests.

Each API component has its own `route` definition and they recursively
build up a `Delayed` computation used to produce the input to the
handler function.

For example, `Capture "id" Int` says to grab the next URL path segment,
try to parse it as an `Int`, then pass it to the handler. Chaining more
API components builds out the `Delayed` computation whose result is
passed to the handler function.

Lastly we have `hoistServerWithContext`. Servant applications are
typically written in a custom monad `m` but ultimately they need to be
run in `Handler`. We use a natural transformation to map from `m` to
`Handler`. This function says how to propagate the natural
transformation through this API component.

## Our instance

Our datatype will contain a symbol representing the auth method to
lookup in the `Context` and a role `r` that we will use to check against
our authenticated user\'s role:

``` haskell
data RequireRole (tag :: Symbol) (r :: k)
```

Our `hoistServerWithContext` definition is boring and just passes the
natural transformation through the next API component.

`route` is where we do all the work. We want to get the `AuthServerData`
from the `AuthHandler` and call `checkRole` to compare the user\'s role
against the required role in the API component.

To make this work we need to create another typeclass that tells us how
to extract a role from our auth context.

``` haskell
class HasRole auth r | auth -> r where
  getRole :: auth -> r

instance HasRole Authz UserRole where
  getRole = userRole
```

Then we fancy up `CheckRole` with an associated type and a `Proxy`:

``` haskell
class CheckRole (r :: k) where
  type RoleType r :: Type

  checkRole :: Proxy r -> RoleType r -> Bool
```

Now inside `route` we can use `Proxy` to specialize `CheckRole` to the
instance `r` and call `checkRole` against the user\'s role from
`getRole`:

``` haskell
checkRole (Proxy @r) (getRole auth)
```

If the role check fails we throw a 403 error. Otherwise we allow the
`Delayed` computation to continue with the `AuthServerData`.

We call execute the failure with `delayedFail`. This makes the error non
fatal allowing Servant to try the next `:<|>` alternative. This gives us
fall through behavior for matching route paths with different role
requirements.

An actual auth lookup failure calls `delayedFailFatal` which returns
immediately. This means we reject all unauthenticated requests.

We could instead ouse `delayedFail` here allowing an unauthenticated
request to fall-through every gate before failing. This would let us
create an unauthenticated fallback route.

This is a design choice, perhaps I should offer combinators with both
behaviors?

The full `HasServer` instance:

``` haskell
instance
  forall tag r api context.
  ( HasServer api context,
    CheckRole r,
    HasRole (AuthServerData (AuthProtect tag)) (RoleType r),
    HasContextEntry context (AuthHandler Request (AuthServerData (AuthProtect tag)))
  ) =>
  HasServer (RequireRole tag r :> api) context
  where
  type
    ServerT (RequireRole tag r :> api) m =
      AuthServerData (AuthProtect tag) -> ServerT api m

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy @api) pc nt . s

  route _ context subserver =
    route (Proxy @api) context (subserver `addAuthCheck` withRequest authCheck)
    where
      authHandler' :: Request -> Handler (AuthServerData (AuthProtect tag))
      authHandler' = unAuthHandler (getContextEntry context)

      authCheck :: Request -> DelayedIO (AuthServerData (AuthProtect tag))
      authCheck req = do
        eResult <- liftIO $ runHandler (authHandler' req)
        case eResult of
          Left err -> delayedFailFatal err
          Right auth ->
            if checkRole (Proxy @r) (getRole auth)
              then pure auth
              else
                delayedFail
                  err403
                    { errBody = "Forbidden: insufficient permissions",
                      errHeaders = [("Content-Type", "text/plain; charset=utf-8")]
                    }
```

Armed with a working servant combinator we can build a working example.

``` haskell
data UserRole = Viewer | Editor | Admin
  deriving (Ord)

instance CheckRole 'Viewer where
  type RoleType 'Viewer = UserRole
  checkRole _ role = role >= Viewer

instance CheckRole 'Editor where
  type RoleType 'Editor = UserRole
  checkRole _ role = role >= Editor

instance CheckRole 'Admin where
  type RoleType 'Admin = UserRole
  checkRole _ role = role >= Admin

data Authz = Authz { userRole :: UserRole, userName :: String }
  deriving (Show)

instance HasRole Authz UserRole where
  getRole = userRole

type instance AuthServerData (AuthProtect "test-auth") = Authz

type PanelAdminAPI =
  RequireRole "test-auth" 'Admin
    :> "panel"
    :> Get '[JSON] String

type PanelEditorAPI =
  RequireRole "test-auth" 'Editor
    :> "panel"
    :> Get '[JSON] String

type PanelViewerAPI =
  RequireRole "test-auth" 'Viewer
    :> "panel"
    :> Get '[JSON] String

type API = PanelAdminAPI :<|> PanelEditorAPI :<|> PanelViewerAPI

server :: Server API
server = panelAdmin :<|> panelEditor :<|> panelViewer
  where
    panelAdmin :: Authz -> Handler String
    panelAdmin _ = pure "admin panel"

    panelEditor :: Authz -> Handler String
    panelEditor _ = pure "editor panel"

    panelViewer :: Authz -> Handler String
    panelViewer _ = pure "viewer panel"
```

Nice!

# Who Authorized Those Ghosts?

This is pretty neat but I think we can do better. We are gating access
to handlers by user roles but we can call any sort of subroutine from
our handlers.

If we constructed a special `Proof` token during the role check we could
constrain what subroutines are callable with a handler.

The `Proof` token would be index by the auth role checked in the
`HasServer` `route` function. Then we could build functions like:

``` haskell
banUser :: Proof 'Admin -> Authz -> String
banUser _proof auth = "banned by " <> userName auth
```

And we would only be able to call this function from an `'Admin`
authscoped handler.

Note that because the `Authz` itself is not (yet!) indexed by the role,
we don\'t have a proof that a ****particular**** user passed the role
check, just that ****some**** user did.

To make this work we need to define a couple data types:

``` haskell
data Proof (required :: k) = Proof

data Satisfies (required :: k) authz = Satisfies (Proof required) authz
```

`Proof` is a witness that some role satifies the requirement `required`.
The constructor is not exported so the only way to construct it will be
via a new function `checkAuth` called in `authCheck` and yields a
`Proof` if the `checkRole` succeeds.

`Satisfies` is an authentication result paired with a proof that it
satisfies `required`. This is what handlers receive.

`checkAuth` replaces the `if` statement in `authCheck` and produces our
`Proof`:

``` haskell
checkAuth :: CheckRole required => Proxy required -> RoleType required -> Maybe (Proof required)
checkAuth p role
  | checkRole p role = Just Proof
  | otherwise = Nothing
```

`authCheck` now returns
`Satisfies required (AuthServerData (AuthProtect tag))`:

``` haskell
authCheck :: Request -> DelayedIO (Satisfies required (AuthServerData (AuthProtect tag)))
authCheck request = do
  eResult <- liftIO $ runHandler (authHandler' request)
  case eResult of
    Left err -> delayedFailFatal err
    Right auth ->
      case checkAuth (Proxy @required) (getRole auth) of
        Just proof -> pure $ Satisfies proof auth
        Nothing ->
          delayedFail
            err403
              { errBody = "Forbidden: insufficient permissions",
                errHeaders = [("Content-Type", "text/plain; charset=utf-8")]
              }
```

Our handlers now receive
`Satisfies required (AuthServerData (AuthProtect tag))` allowing the
admin handler to call `banUser` using the proof:

``` haskell
adminHandler :: Satisfies 'Admin Authz -> Handler String
adminHandler (Satisfies proof auth) = pure (banUser proof auth)
```

The other handlers cannot call `banUser` as they have the wrong proof,
demonstrated by their `Satisfies` param:

``` haskell
editorHandler :: Satisfies 'Editor Authz -> Handler String
editorHandler (Satisfies _proof auth) = pure $ "editor: " <> show (userName auth)

viewerHandler :: Satisfies 'Viewer Authz -> Handler String
viewerHandler (Satisfies _proof auth) = pure $ "viewer: " <> show (userName auth)
```

We have restricted subroutines to handlers with the approriate role
check. What we have not done is restrict those subroutine to
****users**** who have satisifed those role checks. Inside
`editorHandler` we could fetch a `Authz` for some other user and use it
with the `Proof 'Admin` we have acquired.

To prevent this we need to lift the `UserRole` from a value inside the
`Authz` into an index on `UserRole`:

``` haskell
newtype Authz (r :: UserRole) = Authz {userName :: String}
```

In other words, currently the Proof and the subject are independent. We
have proven ****someone**** is an admin (or whatever) but we don\'t have
evidence of who.

It turns out this will require taking a few steps into the
[hasochism](https://homepages.inf.ed.ac.uk/slindley/papers/hasochism.pdf).

# (Don\'t fear) The Reaper

The remainder gets a little brutal. I\'m going to walk through the
minimal singletons based solution without all the bells and whistles and
with a bunch of boilerplate. In the actual library `servant-auth-roles`
I take a slightly different singletons approach and hide it all behind
Template Haskell to eliminate the boilerplate.

# Reification and forgery

The foundational concept in singletons is that we construct a GADT that
uniquely maps to indices of an indexed type. This allows you to recover
type level information at runtime via pattern matching on the GADT.

``` haskell
data SBool (b :: Bool) where
  STrue  :: SBool 'True
  SFalse :: SBool 'False
```

With `SBool`, each branch of the GADT uniquely specializes `b` to a type
of the `Bool` kind. This means that when pattern matching GHC is able to
infer `b` based on the branch.

This allows us to perform reification, mapping from a value to a type.

Imagine trying to write this function:

``` haskell
toSBool :: Bool -> SBool b
```

No matter which case of `SBool` you return, you will get a type error
because `b` is universally quantified. However, if we hide the parameter
behind an existential quantifier then we can write:

``` haskell
data SomeSBool where
  SomeSBool :: SBool b -> SomeSBool

toSBool :: Bool -> SomeSBool
toSBool True  = SomeSBool STrue
toSBool False = SomeSBool SFalse
```

And now when we pattern match on `SomeBool` we can access the
specialized `b`. This allows us to write a function like:

``` haskell
fireTheMissiles :: SBool 'True -> IO ()
fireTheMissiles STrue = print "Missiles have been fired!"

check :: Bool -> IO ()
check input = case toSBool input of
  SomeSBool tru  -> fireTheMissiles tru
  SomeSBool SFalse -> pure ()
```

Calling `fireTheMissiles` with `SFalse` will result in a type error as
it demands `SBool 'True`. This is the vibe of the trick we want to play.
However, at this point could forge an `STrue` in the `SFalse` branch:

``` haskell
SomeSBool SFalse  -> fireTheMissiles STrue
```

To prevent a forgery we need to introduce an evidence type that shares
the index `b`. This way we can enforce that our `SBool` matches that of
our evidence.

``` haskell
data IsTrue (b :: Bool) where
  IsTrue :: IsTrue 'True

decide :: SBool b -> Maybe (IsTrue b)
decide STrue  = Just IsTrue
decide SFalse = Nothing

fireTheMissiles :: IsTrue b -> SBool b -> IO ()
fireTheMissiles = ...
```

The only way to call `fireTheMissiles` is if our `SBool b` matches our
`IsTrue b`:

``` haskell
check :: Bool -> IO ()
check input = case toSBool input of
  SomeSBool sb -> case decide sb of
    Just ev -> fireTheMissiles ev sb
    Nothing -> pure ()
```

Now we can only call `fireTheMissiles` on `input` if we have also
produced evidence that `input` is `True`, which we do with `decide`.

Let\'s look at our `banUser` function:

``` haskell
banUser :: Proof 'Admin -> Authz -> String
```

`Proof 'Admin` is our evidence type. If we parameterize it and `Authz`
with `r :: UserRole` then `banUser` has exactly the same shape as
`fireTheMissiles`:

``` haskell
banUser :: Proof 'Admin r -> Authz r -> String
```

We can only call `banUser` with an `Authz` that matches `Proof`.

# Applying singletons

Extrapolating from the `SBool` example, we have three key changes:

1.  We need to dispatch our check function on a type, since the required
    role is a parameter of `RequireRole` rather than a fixed value like
    `'True`.
2.  We need to construct the proof inside the `HasServer` `route`
    method.
3.  We need the proof and the `Authz` it was checked against to travel
    together from `route` to our handler.

## Dispatch

We need to update `checkRole` to follow the shape of `decide`. However,
since the typeclass is polymorphic in the role kind we cannot expect a
particular singleton GADT. We need to dispatch a singleton on the kind
of the `RoleType` associated type.

For that we introduce the `Sing` type family.

``` haskell
type family Sing :: k -> Type
```

Put simply, it is a type level function that allows us to uniquely pick
out a singleton type for a kind.

`Sing` dispatches on the kind of its parameter. So given a `SUserRole`
GADT and instance:

``` haskell
data SUserRole (r :: UserRole) where
  SViewer :: SUserRole 'Viewer
  SEditor :: SUserRole 'Editor
  SAdmin :: SUserRole 'Admin

type instance Sing = SUserRole
```

`Sing (r :: UserRole)` must dispatch to `SUserRole`. If there are
instances for multiple GADTs on the same kind then GHC will produce a
conflicting instance error.

Now in our `checkRole` method we replace `RoleType required` with its
singleton via `Sing`.

``` haskell
class CheckRole (required :: k) where
  -- | The value-level type that this role checks against.
  type RoleType required :: Type

  -- | Check whether the given role value satisfies the requirement @required@.
  checkRole :: Sing (actual :: RoleType required) -> ...
```

For `CheckRole 'Viewer`, `RoleType 'Viewer` resolves to `UserRole`
giving us `Sing (actual :: UserRole)` which we just showed returns
`SUserRole`.

## Evidence

Now we must update our `Proof` type and complete the signature for
`checkRole`. `Proof` currently carries the required auth role.

Recall that in our current version the user provides
`checkRole :: Proxy required -> RoleType required -> Bool` and then the
library function `checkAuth` acts as a smart constructor to build the
appropriate `Proof`:

``` haskell
checkAuth :: CheckRole required => Proxy required -> RoleType required -> Maybe (Proof required)
checkAuth p role
  | checkRole p role = Just Proof
  | otherwise = Nothing
```

This allows us to keep the `Proof` data constructor out of user space to
eliminate forgery.

In the new world we don\'t need this.

Lets take a step back and think about what proof we need.

1.  The role the route demands.
2.  The role the user has.

1 is how we dispatch a proof to a handler and 2 is the actual role of
the authorized user, which ties the proof to the subject it was checked
against.

``` haskell
data Proof (required :: kr) (actual :: ka) where
  Proof :: Proof required actual
```

`required` is the role required by the route and `actual` is the auth\'d
user\'s role.

Now we can complete our `checkRole` signature, dropping the proxy.

``` haskell
checkRole :: Sing (actual :: RoleType required) -> Maybe (Proof required actual)
```

And we can write an instance like this:

``` haskell
instance CheckRole 'Editor where
  checkRole SViewer = Nothing
  checkRole SEditor = Just Proof
  checkRole SAdmin  = Just Proof
```

The required role is `'Editor` so `Sing (RoleType required)` evaluates
to `SUserRole _` representing the actual role of the authenticated user.
If it is `SViewer` then we reject with `Nothing`, otherwise we produce a
`Just Proof` which is specialized to `Proof 'Editor actual`.

In the `SEditor` branch, `actual` is specialized to `'Editor` giving us
`Proof 'Editor 'Editor`.

Now, were we to construct a proof in the `SViewer` branch it would be
`Proof 'Editor 'Viewer`. This would be morally wrong as the point of the
proof is that the actual satisfies the role requirement. We need to
eliminate the ability to produce such a proof.

We can do that with another type family.

``` haskell
type family Satisfied (required :: kr) (actual :: ka) :: Constraint
```

`Satisfied` is the constraint that holds when `actual` meets the
satisfaction condition for `required`. That condition is user defined
(equality, set membership, order, etc) so I\'m going to defer an example
for a moment.

Now we attach that constraint on `Proof` and we can gaurantee that the
satisfaction condition holds when constructing a proof.

``` haskell

data Proof (required :: kr) (actual :: ka) where
  Proof :: (Satisfied required actual) => Proof required actual
```

Now `checkRole SViewer = Just Proof` would demand
`Satisfied 'Editor 'Viewer` which wont typecheck.

This also means we no longer have to worry about hiding the `Proof` data
constructor. There is no way for a user to construct a proof whose
`actual` role fails to satisfy the requirement. Thus we can also
eliminate the `checkAuth` library function.

## Constructing

Now we need to update our `HasServer` instance using the new `CheckRole`
class. To do this we need to reify the role type from the request value.

We do this with another GADT `SomeRole` which works just like
`SomeSBool`.

``` haskell
data SomeRole (authF :: k -> Type) where
  SomeRole :: Sing (r :: k) -> authF r -> SomeRole authF
```

The only real difference is that we also pack `SomeRole` with the auth
value along with the singleton. We construct the `SomeRole` value in our
`AuthHandler` and then discharge it in the `HasServer` instance.

Introduction:

``` haskell
parseRole :: Request -> Maybe (SomeRole Authz)
parseRole req = case lookup "X-Role" (requestHeaders req) of
  Just "viewer" -> pure $ SomeRole SViewer (Authz "Reed")
  Just "editor" -> pure $ SomeRole SEditor (Authz "Lyxia")
  Just "admin" -> pure $ SomeRole SAdmin (Authz "Sandy")
  _ -> Nothing

-- | Reads the "X-Role" header to determine the user's role.
-- Missing header -> 401. Values: "viewer", "editor", "admin".
roleAuthHandler :: AuthHandler Request (SomeRole Authz)
roleAuthHandler = mkAuthHandler $ \req ->
  maybe (throwError err401) pure (parseRole req)
```

Elimination:

``` haskell
authCheck :: Request -> DelayedIO (Satisfies required authF)
authCheck request = do
  eResult <- liftIO $ runHandler (authHandler' request)
  case eResult of
    Left err -> delayedFailFatal err
    Right (SomeRole sActual auth) ->
      case checkRole sActual of
        Just proof -> pure $ Satisfies proof auth
        Nothing ->
          delayedFail
            err403
              { errBody = "Forbidden: insufficient permissions",
                errHeaders = [("Content-Type", "text/plain; charset=utf-8")]
              }
```

`Satisfies` becomes the final GADT which carries the associated `Proof`
the auth object to be propagated to our handler.

``` haskell
data Satisfies (required :: kr) (authF :: ka -> Type) where
  Satisfies :: Proof required actual -> authF actual -> Satisfies required authF
```

## Discharging

We still need to define our `Satisfied` instance for `UserRole`.

``` haskell
type family (a :: UserRole) <=? (b :: UserRole) :: Bool where
  'Viewer <=? _       = 'True
  'Editor <=? 'Viewer = 'False
  'Editor <=? _       = 'True
  'Admin  <=? 'Admin  = 'True
  'Admin  <=? _       = 'False

type instance Satisfied (required :: UserRole) (actual :: UserRole) =
  (required <=? actual) ~ 'True
```

`<=?` is a type level Ord specialized to `UserRole`. `Viewer` matches
anything, `Editor` matches itself or `Admin`, `Admin` matches itself.
Then in `Satisfied`, if `required <=? actual` evaluates to `'True` then
the whole constraint evalues to `'True ~ 'True`{.verbatim} and holds.

And at last we are back to our servant handlers. They receive a
`Satisfies 'Admin Authz` which contains the `Proof` and the `Authz` both
at the same `r`, so the proof can only be spent on the very `Authz` it
was checked against.

This allows us to write `banUser` such that it can only be used in
authorized route by an authorized user.

``` haskell
banUser :: Proof 'Admin r -> Authz r -> String
banUser _prf auth = "banned by " <> userName auth
```

# Conclusion

Wow that was brutal. Writing dependent Haskell is painful. Luckily a ton
of this code is boilerplate that can be automated away via template
haskell, which i have done in my libary
[servant-auth-roles](https://github.com/solomon-b/servant-auth-roles).

The complete `UserRole` example becomes:

``` haskell
data UserRole = Viewer | Editor | Admin

$(deriveOrdRole ''UserRole)

newtype Authz (r :: UserRole) = Authz {userName :: String}

type instance AuthServerData (AuthProtect "role-auth") = SomeRole Authz

parseRole :: Request -> Maybe (SomeRole Authz)
parseRole req = case lookup "X-Role" (requestHeaders req) of
  Just "viewer" -> Just (someUserRole Viewer (Authz "Reed"))
  Just "editor" -> Just (someUserRole Editor (Authz "Lyxia"))
  Just "admin" -> Just (someUserRole Admin (Authz "Sandy"))
  _ -> Nothing

roleAuthHandler :: AuthHandler Request (SomeRole Authz)
roleAuthHandler = mkAuthHandler $ maybe (throwError err401) pure . parseRole

banUser :: (IsAtleastAdmin userAuth) => Authz userAuth -> String
banUser auth = "banned by " <> userName auth

type PanelAdminAPI = RequireRole "role-auth" 'Admin :> "panel" :> Get '[JSON] String

type PanelEditorAPI = RequireRole "role-auth" 'Editor :> "panel" :> Get '[JSON] String

type PanelViewerAPI = RequireRole "role-auth" 'Viewer :> "panel" :> Get '[JSON] String

type FallthroughAPI = PanelAdminAPI :<|> PanelEditorAPI :<|> PanelViewerAPI

server :: Server FallthroughAPI
server = pAdmin :<|> pEditor :<|> pViewer
  where
    pAdmin :: Satisfies 'Admin Authz -> Handler String
    pAdmin (Satisfies UserRoleProof auth) = pure (banUser auth)

    pEditor :: Satisfies 'Editor Authz -> Handler String
    pEditor _ = pure "editor panel"

    pViewer :: Satisfies 'Viewer Authz -> Handler String
    pViewer _ = pure "viewer panel"

app :: IO Application
app = pure $ serveWithContext (Proxy @FallthroughAPI) (roleAuthHandler :. EmptyContext) server
```

No singletons or weird boilerplate in sight!

The hierarchy is derived via `Ord`. Equality based roles and Set
Membership based roles can be derived with `deriveEqRole` and
`deriveMemberRole`.
