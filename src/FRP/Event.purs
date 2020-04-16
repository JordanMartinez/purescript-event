module FRP.Event
  ( Event
  , EventIO
  , create
  , makeEvent
  , subscribe
  , module Class
  ) where

import Prelude

import Control.Alternative (class Alt, class Alternative, class Plus)
import Control.Apply (lift2)
import Data.Array (deleteBy)
import Data.Either (either, fromLeft, fromRight, hush, isLeft, isRight)
import Data.Compactable (class Compactable)
import Data.Filterable (class Filterable, filterMap)
import Data.Foldable (sequence_, traverse_)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Effect (Effect)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event.Class (class Filterable, class IsEvent, count, filterMap, fix,
                        fold, folded, gate, gateBy, keepLatest, mapAccum,
                        sampleOn, sampleOn_, withLast) as Class
import Partial.Unsafe (unsafePartial)
import Unsafe.Reference (unsafeRefEq)

-- | An `Event` represents a collection of discrete occurrences with associated
-- | times. Conceptually, an `Event` is a (possibly-infinite) list of values-and-times:
-- |
-- | ```purescript
-- | type Event a = List { value :: a, time :: Time }
-- | ```
-- |
-- | Events are created from real events like timers or mouse clicks, and then
-- | combined using the various functions and instances provided in this module.
-- |
-- | Events are consumed by providing a callback using the `subscribe` function.
newtype Event m a = Event ((a -> m Unit) -> m (m Unit))

instance functorEvent :: Functor (Event m) where
  map f (Event e) = Event \k -> e (k <<< f)

instance compactableEvent :: MonadEffect m => Compactable (Event m) where
  compact xs = map (\x -> unsafePartial fromJust x) (filter isJust xs)
  separate xs =
    { left: unsafePartial (map fromLeft) (filter isLeft xs)
    , right: unsafePartial (map fromRight) (filter isRight xs)
    }

instance filterableEvent :: MonadEffect m => Filterable (Event m) where
  filter = filter

  filterMap f = map (\x -> unsafePartial fromJust x) <<< filter isJust <<< map f

  partition p xs = { yes: filter p xs, no: filter (not <<< p) xs }

  partitionMap f xs =
    { left: filterMap (either Just (const Nothing) <<< f) xs
    , right: filterMap (hush <<< f) xs
    }

instance applyEvent :: MonadEffect m => Apply (Event m) where
  apply (Event e1) (Event e2) = Event \k -> do
    latestA /\ latestB <- liftEffect do
      latestA <- Ref.new Nothing
      latestB <- Ref.new Nothing
      pure (latestA /\ latestB)
    c1 <- e1 \a -> do
      b <- liftEffect do

        Ref.write (Just a) latestA
        Ref.read latestB
      traverse_ (k <<< a) b
    c2 <- e2 \b -> do
      a <- liftEffect do
        Ref.write (Just b) latestB
        Ref.read latestA
      traverse_ (k <<< (_ $ b)) a
    pure (c1 *> c2)

instance applicativeEvent :: MonadEffect m => Applicative (Event m) where
  pure a = Event \k -> do
    k a
    pure (pure unit)

instance altEvent :: Monad m => Alt (Event m) where
  alt (Event f) (Event g) = Event \k -> do
    c1 <- f k
    c2 <- g k
    pure (c1 *> c2)

instance plusEvent :: MonadEffect m => Plus (Event m) where
  empty = Event \_ -> pure (pure unit)

instance alternativeEvent :: MonadEffect m => Alternative (Event m)

instance semigroupEvent :: (MonadEffect m, Semigroup a) => Semigroup (Event m a) where
  append = lift2 append

instance monoidEvent :: (MonadEffect m, Monoid a) => Monoid (Event m a) where
  mempty = pure mempty

instance eventIsEvent :: MonadEffect m => Class.IsEvent (Event m) where
  fold = fold
  keepLatest = keepLatest
  sampleOn = sampleOn
  fix = fix

-- | Fold over values received from some `Event`, creating a new `Event`.
fold :: forall m a b. MonadEffect m => (a -> b -> b) -> Event m a -> b -> Event m b
fold f (Event e) b = Event \k -> do
  result <- liftEffect $ Ref.new b
  e \a -> do
    rez <- liftEffect $ Ref.modify (f a) result
    k rez

-- | Create an `Event` which only fires when a predicate holds.
filter :: forall m a. Applicative m => (a -> Boolean) -> Event m a -> Event m a
filter p (Event e) = Event \k -> e \a -> if p a then k a else pure unit

-- | Create an `Event` which samples the latest values from the first event
-- | at the times when the second event fires.
sampleOn :: forall m a b. MonadEffect m => Event m a -> Event m (a -> b) -> Event m b
sampleOn (Event e1) (Event e2) = Event \k -> do
  latest <- liftEffect $ Ref.new Nothing
  c1 <- e1 \a -> do
    liftEffect $ Ref.write (Just a) latest
  c2 <- e2 \f -> do
    latest0 <- liftEffect $ Ref.read latest
    traverse_ (k <<< f) latest0
  pure (c1 *> c2)

-- | Flatten a nested `Event`, reporting values only from the most recent
-- | inner `Event`.
keepLatest :: forall m a. MonadEffect m => Event m (Event m a) -> Event m a
keepLatest (Event e) = Event \k -> do
  cancelInner <- liftEffect $ Ref.new Nothing
  cancelOuter <- e \inner -> do
    (liftEffect $ Ref.read cancelInner) >>= sequence_
    c <- subscribe inner k
    liftEffect $ Ref.write (Just c) cancelInner
  pure do
    (liftEffect $ Ref.read cancelInner) >>= sequence_
    cancelOuter

-- | Compute a fixed point
fix :: forall m i o. MonadEffect m => (Event m i -> { input :: Event m i, output :: Event m o }) -> Event m o
fix f = Event \k -> do
    { event, push } <- create
    let { input, output } = f event
    c1 <- subscribe input push
    c2 <- subscribe output k
    pure (c1 *> c2)

-- | Subscribe to an `Event` by providing a callback.
-- |
-- | `subscribe` returns a canceller function.
subscribe
  :: forall r m a
   . MonadEffect m
  => Event m a
  -> (a -> m r)
  -> m (m Unit)
subscribe (Event e) k = e (void <<< k)

-- | Make an `Event` from a function which accepts a callback and returns an
-- | unsubscription function.
-- |
-- | Note: you probably want to use `create` instead, unless you need explicit
-- | control over unsubscription.
makeEvent
  :: forall m a
   . ((a -> m Unit) -> m (m Unit))
  -> Event m a
makeEvent = Event

type EventIO m a =
  { event :: Event m a
  , push :: a -> m Unit
  }

-- | Create an event and a function which supplies a value to that event.
create
  :: forall m a
   . MonadEffect m
  => m (EventIO m a)
create = do
  subscribers <- liftEffect $ Ref.new []
  pure
    { event: Event \k -> do
        _ <- liftEffect $ Ref.modify (_ <> [k]) subscribers
        pure do
          _ <- liftEffect $ Ref.modify (deleteBy unsafeRefEq k) subscribers
          pure unit
    , push: \a -> do
        (liftEffect $ Ref.read subscribers) >>= traverse_ \k -> k a
    }
