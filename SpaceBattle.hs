module SpaceBattle
( predict
) where

import Fleet
import Binomial
import RollCombatDice
import Data.Function.Memoize

-- Given an initial battle fleet of units (them, us)
-- Return (probability of eliminating enemy, probability of survival, expected value)

type Outcome = (Double, Double, Double)

probAdj ::  Outcome -> Double -> Outcome -> Outcome
probAdj (oa, ob, oc) p (a, b, c) = (oa+p*a, ob+p*b, oc+p*c)

tally :: (Fleets -> Outcome) ->   -- Memoized prediction function (curried recursion)
         Fleets              ->   -- The fleet before this outcome
         Outcome             ->   -- Tuple of outcome thus far
         (Double, Fleets)    ->   -- One possible outcome
         Outcome                  -- Revised tuple of outcome
tally func before (e, s, v) (prob, (us, them)) = 
  if them == []
    then if us == []
           then (e + prob, s,      v + prob*(valueDiff before (us, them))) -- Mutual annihilation 
           else (e + prob, s+prob, v + prob*(valueDiff before (us, them))) -- Eliminated and survived
    else if us == []
           then (e,        s,      v + prob*(valueDiff before (us, them))) -- Wiped out
           else probAdj (e, s, v) prob (func (us, them)) -- Another round


-- This is memoized, to vastly improve computational time
predict :: Fleets -> Outcome
predict = memoFix $ \wins' fleets -> foldl (tally wins' fleets) (0.0, 0.0, 0.0) (outcomes fleets)


-- Given an initial battle set of units (us, them)
-- Return (probability of eliminating enemy, probability of survival, expected value)
predictSabotageRun :: Int -> (Fleet, Fleet) -> Outcome
predictSabotageRun _ _ = (0.0, 0.0, 0.0)

-- FIXME: Assault Cannon Technology
--- before space battle Dreadnoughts may each fire one shot, immediate casualties no return fire!!!

-- FIXME: What about racial combat bonuses?


