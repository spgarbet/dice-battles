module Sabotage where

import Fleet
import SpaceBattle
import RollCombatDice
import Binomial
import Text.Show.Pretty
import Data.List
import Debug.Trace

-- z = ([(Carrier, 1), (Fighter, 6)], [(WarSun,  1), (Fighter, 4), (Cruiser, 2)]) :: Fleets
-- z' = ([(Cruiser, 1),(Carrier, 1), (Fighter, 6)], [(WarSun,  1), (Fighter, 4), (Cruiser, 2)]) :: Fleets
-- z'' = ([(Cruiser, 2),(Carrier, 1), (Fighter, 6)], [(WarSun,  1), (Fighter, 4), (Cruiser, 2)]) :: Fleets
-- z n = ([(Cruiser, n),(Carrier, 1), (Fighter, 6)], [(WarSun,  1), (Fighter, 4), (Cruiser, 2)]) :: Fleets

partitionFighters :: Fleet -> (Fleet, Fleet)
partitionFighters = partition (\(u,_) -> Fleet.sabotage u)

splitUnits :: Fleet -> Int -> (Fleet, Fleet)
splitUnits f 0 = ([], f)
splitUnits [] _ = error "Empty list to split units"
splitUnits ((unit, count):fs) n =
    if n < count
      then ( [(unit, n)], (unit, count-n):fs )
      else if n == count
             then ( [(unit, n)], fs )
             else ( (unit, count):a, b )
    where (a,b) = splitUnits fs (n-count)

takeUnits :: Fleet -> Int -> Fleet
takeUnits f n = fst $ splitUnits f n

removeSabotageHit :: Fleet -> Fleet
removeSabotageHit [] = []
removeSabotageHit ((unit,count):fs)  = 
    case unit of
        WarSun        -> if 1 < count
                           then ((WarSun,count-1):fs)
                           else if 1 == count
                                  then fs
                                  else removeSabotageHit fs  -- This shouldn't happen
        DamagedWarSun -> if 1 < count
                           then ((DamagedWarSun,count - 1):fs)
                           else if 1 == count
                                  then fs
                                  else removeSabotageHit fs -- Shouldn't happen
        _ -> (unit,count):(removeSabotageHit fs)

sabotageOutcomes :: Fleets -> Int -> [(Int, Probability, Fleets)]
sabotageOutcomes (attacker, defender) n = map g sp
    where (f,main)    = partitionFighters attacker   -- take out the fighters
          (run,rejoin)= splitUnits f n          -- split out n fighters
          attacker'   = sort $ rejoin ++ main  -- rejoin the uncommitted
          sp          = zip [0..n] $ dsabotage n
          success     = removeSabotageHit (sort defender)
          g (k,p)     = if k == 0
			              then (k, p, (attacker', defender))
						  else (k, p, (attacker'++(takeUnits run k), success))

sabotageValues :: Fleets -> Int -> [(Int, Probability, Outcome)]
sabotageValues f n = map (\(k, p, x) -> (k, p, predict x)) (sabotageOutcomes f n)

sumOutcome :: Outcome -> (Probability, Outcome) -> Outcome
sumOutcome (a,b,c,d) (p,(e,f,g,h)) = (a+p*e, b+p*f, c+p*g, d+p*h)

-- Subtract ships lost to Sabotage
valueShips :: Int -> Int -> Probability -> Double
valueShips n k w = -0.5*(fromIntegral (n-k))

adjValue :: Int -> (Int, Probability, Outcome ) -> (Probability, Outcome)
adjValue n (k, p, (w,m,l,v)) = if k > 0
	                             then (p, (w, m, l, v+(valueShips n k w)+12.0))
								 else (p, (w, m, l, v+(valueShips n k w)))

sabotage :: Fleets -> Int -> Outcome
sabotage fl n = foldl sumOutcome (0.0, 0.0, 0.0, 0.0) $ map (adjValue n) values
    where values      = sabotageValues fl n
          
