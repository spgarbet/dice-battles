module SpaceBattle
( predict
, Outcome
) where

import Fleet
import Binomial
import RollCombatDice
import Data.Function.Memoize

-- Given an initial battle fleet of units (them, us)
-- Return (probability of eliminating enemy, probability of survival, expected value)

type Outcome = (Double, Double, Double)

tally :: (Fleets -> Outcome) ->   -- Memoized prediction function (curried recursion)
         Fleets              ->   -- The fleet before this outcome
         Outcome             ->   -- Tuple of outcome thus far
         (Double, Fleets)    ->   -- One possible outcome
         Outcome                  -- Revised tuple of outcome
tally func before (e, s, v) (prob, after) =
    case after of
        ([], []) -> (e + prob,    s,           vd)  -- mutual destruction
        (_,  []) -> (e + prob,    s + prob,    vd)  -- win
        ([], _ ) -> (e,           s,           vd)  -- loss
        _        -> (e + prob*e', s + prob*s', vd + prob*vd') -- continue
    where vd = v + prob *(valueDiff before after)
          (e', s', vd') = func after

-- This is memoized, to vastly improve computational time
predict :: Fleets -> Outcome
predict = memoFix $ \wins' fleets -> foldl (tally wins' fleets) (0.0, 0.0, 0.0) (outcomes fleets)

-- Given an initial battle set of units (us, them)
-- Return (probability of eliminating enemy, probability of survival, expected value)
-- predictSabotageRun :: Int -> (Fleet, Fleet) -> Outcome
-- predictSabotageRun _ _ = (0.0, 0.0, 0.0)

-- FIXME: Assault Cannon Technology
--- before space battle Dreadnoughts may each fire one shot, immediate casualties no return fire!!!

-- FIXME: What about racial combat bonuses?


