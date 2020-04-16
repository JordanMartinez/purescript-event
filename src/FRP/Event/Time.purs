module FRP.Event.Time
  ( withTime
  , debounce
  , debounceWith
  ) where

import Prelude

import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Time.Duration (Milliseconds)
import Effect.Now (now)
import Effect.Class (class MonadEffect, liftEffect)
import FRP.Event (Event, makeEvent, subscribe)
import FRP.Event.Class (fix, gateBy)

-- | Create an event which fires every specified number of milliseconds.
-- Note: this could not be ported because 'setInterval' requires
-- the callback to run in the Effect monad, not our custom monad.
-- interval :: forall m. MonadEffect m => Int -> Event m Instant
-- interval n = makeEvent \k -> do
--   id <- liftEffect $ setInterval n do
--     time <- liftEffect $ now
--     k time
--   pure (liftEffect $ clearInterval id)

-- | Create an event which reports the current time in milliseconds since the epoch.
withTime :: forall m a. MonadEffect m => Event m a -> Event m { value :: a, time :: Instant }
withTime e = makeEvent \k ->
  subscribe e \value -> do
    time <- liftEffect $ now
    k { time, value }

-- | On each event, ignore subsequent events for a given number of milliseconds.
debounce :: forall m a. MonadEffect m => Milliseconds -> Event m a -> Event m a
debounce period = debounceWith (map { period, value: _ })

-- | Provided an input event and transformation, block the input event for the
-- | duration of the specified period on each output.
debounceWith
  :: forall m a b
   . MonadEffect m
  => (Event m a -> Event m { period :: Milliseconds, value :: b })
  -> Event m a
  -> Event m b
debounceWith process event
  = fix \allowed ->
      let
        processed :: Event m { period :: Milliseconds, value :: b }
        processed = process allowed

        expiries :: Event m Instant
        expiries =
          map (\{ time, value } -> fromMaybe time (instant (unInstant time <> value)))
              (withTime (map _.period processed))

        comparison :: forall r. Maybe Instant -> { time :: Instant | r } -> Boolean
        comparison a b = maybe true (_ < b.time) a

        unblocked :: Event m { time :: Instant, value :: a }
        unblocked = gateBy comparison expiries stamped
      in
        { input:  map _.value unblocked
        , output: map _.value processed
        }
  where
    stamped :: Event m { time :: Instant, value :: a }
    stamped = withTime event
