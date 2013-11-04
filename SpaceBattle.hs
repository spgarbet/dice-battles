module SpaceBattle
( predict
, Outcome
) where

import Fleet
import Binomial
import RollCombatDice
import Data.Function.Memoize

-- Given an initial battle fleet of units (us, them)
-- Return (us victory prob, mutual destruction prob,  them victory prob, expected value)
type Outcome = (Double, Double, Double, Double)

tally :: (Fleets -> Outcome) ->   -- Memoized prediction function (curried recursion)
         Fleets              ->   -- The fleet before this outcome
         Outcome             ->   -- Tuple of outcome thus far
         (Double, Fleets)    ->   -- One possible outcome
         Outcome                  -- Revised tuple of outcome
tally func before (u, m, t, v) (prob, after) =
    case after of
        ([], []) -> (u,           m+prob,      t,           vd)  -- mutual destruction
        (_,  []) -> (u + prob,    m,           t,           vd)  -- us win
        ([], _ ) -> (u,           m,           t + prob,    vd)  -- them win
        _        -> (u + prob*u', m+prob*m',   t + prob*t', vd + prob*vd') -- down the rabbit hole
    where vd = v + prob*(valueDiff before after)
          (u', m', t', vd') = func after

-- This is memoized, to vastly improve computational time
predict :: Fleets -> Outcome
predict = memoFix $ \wins' fleets -> foldl (tally wins' fleets) (0.0, 0.0, 0.0, 0.0) (outcomes fleets)


-- FIXME: Assault Cannon Technology
--- before space battle Dreadnoughts may each fire one shot, immediate casualties no return fire!!!

-- FIXME: What about racial combat bonuses?


