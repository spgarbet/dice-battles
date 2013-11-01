import Fleet
import Binomial
import RollCombatDice

-- Given an initial battle fleet of units (them, us)
-- Return (probability of eliminating enemy, probability of survival, expected value)
predictSpaceBattle :: (Fleet, Fleet) -> (Double, Double, Double)
predictSpaceBattle (them, us) = sortedPredictSpaceBattle (sort them, sort us)

sortedPredictSpaceBattle = memoize predictSpaceBattle'

predictSpaceBattle' :: (Fleet, Fleet) -> (Double, Double, Double)
predictSpaceBattle' = (0,0,0)


-- Given an initial battle set of units (us, them)
-- Return (probability of eliminating enemy, probability of survival, expected value)
predictSabotageRun :: Int -> (Fleet, Fleet) -> (Double, Double, Double)
predictSabotageRun _ _ = (0.0, 0.0, 0.0)

-- FIXME: Assault Cannon Technology
--- before space battle Dreadnoughts may each fire one shot, immediate casualties no return fire!!!

-- FIXME: What about racial combat bonuses?