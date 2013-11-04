module SpaceBattle
( predict
, Outcome
) where

import Fleet
import Binomial
import RollCombatDice
import Data.Function.Memoize

-- Given an initial battle fleet of units (attacker, defender)
-- Return (attacker victory prob, mutual destruction prob,  defender victory prob, expected value)
type Outcome = (Double, Double, Double, Double)

tally :: (Fleets -> Outcome) ->   -- Memoized prediction function (curried recursion)
         Fleets              ->   -- The fleet before this outcome
         Outcome             ->   -- Tuple of outcome thattacker far
         (Double, Fleets)    ->   -- One possible outcome
         Outcome                  -- Revised tuple of outcome
tally func before (u, m, t, v) (prob, after) =
    case after of
        ([], []) -> (u,           m+prob,      t,           vd)  -- mutual destruction
        (_,  []) -> (u + prob,    m,           t,           vd)  -- attacker win
        ([], _ ) -> (u,           m,           t + prob,    vd)  -- defender win
        _        -> (u + prob*u', m+prob*m',   t + prob*t', vd + prob*vd') -- down the rabbit hole
    where vd = v + prob*(valueDiff before after)
          (u', m', t', vd') = func after

-- This is memoized, to vastly improve computational time
predict :: Fleets -> Outcome
predict = memoFix $ \wins' fleets -> foldl (tally wins' fleets) (0.0, 0.0, 0.0, 0.0) (outcomes fleets)

-- 1. PDS fire, against space ships,   defender first, attacker second
-- 2. anti-fighter barrage, 2 dice for each destroyer, every hit eliminates a Fighter
-- 2. assault cannons, -- Each Dreadnought fires one shot pre-combat
-- 3. mentak ability (pre-fire with up to 2 cruisers or destroyers), immediate casualties
-- 4. Minister of War -- One Auto Hit
-- 5. Sabotage Run
-- 6. Main combat  (What about Nekro Virus?, where does their ship fall)
-- 7. Landing attacking GFs declared
-- 8. X-89 Bacterial Weapon, eliminate all GFs of defender (must have Dreadnaught or WarSun)
-- 9. Planetary Bombardment, Dreadnoughts and WarSuns may bombard a planet (normal battle dice), removes GF, but ONLY if no PDS on planet (unless Graviton Negator by attacker)
-- 10. Return PDS fire against invading GFs
-- 11. Combat dice for all GFs. (Graviton Negator allows Fighters to count)
-- 12. Upgrade surviving troops if they are "shock troops", a shock must always have a gf
-- 13. Any shock troops capture Space Docks and PDS of enemy!


-- Note: Jol-Nar (-1 to all rolls during space battles and invasion)
-- L1z1x : dreadnoughts +1 space battles, ground forces +1 

