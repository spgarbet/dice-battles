import Fleet
import SpaceBattle
import Binomial
import Text.Show.Pretty

  -----------------------------------------------------------------
 -- Simple example of pitting fighters versus a War Sun
--
fighter n = predict d where
             d = ([(Fighter, n)], [(WarSun, 1)] )
fighterVsWarSun = putStr $ ppShow $ map fighter [1..10]

-- Warning above about 60 units on a side, the round-off error destroys the validity of any results


  -----------------------------------------------------------------
 -- Bigger example w/ Sabotage!
--
-- 1 carrier with 6 fighters, 2 cruisers and dreadnought vs. War sun, 4 fighters, 2 cruisers
--
-- Define fleets left to fight based on number committed to sabotage out of 6 fighters
---- sabotageLost n = ([(Carrier,     1), (Fighter, 6-n), (Cruiser, 2)              ],
----                   [(Dreadnought, 1), (WarSun,  1),   (Fighter, 4), (Cruiser, 2)] )
---- sabotageDmg  n = ([(Carrier,     1), (Fighter, 6-n), (Cruiser, 2)              ],
----                   [(Dreadnought, 1), (DamagedWarSun,  1), (Fighter, 4), (Cruiser, 2)] )
---- sabotageWon  n = ([(Carrier,     1), (Fighter, 6-n), (Cruiser, 2)              ],
----                   [(Dreadnought, 1), (Fighter, 4),   (Cruiser, 2)] )                

sabotageLost n = ([(Carrier,     1), (Fighter, 6-n)              ],
                [(WarSun,  1),   (Fighter, 4), (Cruiser, 2)] )
sabotageDmg  n = ([(Carrier,     1), (Fighter, 6-n)             ],
                [(DamagedWarSun,  1), (Fighter, 4), (Cruiser, 2)] )
sabotageWon  n = ([(Carrier,     1), (Fighter, 6-n)             ],
                [(Fighter, 4),   (Cruiser, 2)] )

-- Binomial hit probabilities of sabotage based on number committed
zeros = 0:zeros :: [Double]
sabotageRunHitProbabilityMass n = (dbinom n 0.02) ++ zeros

tmul :: Probability -> Outcome -> Outcome
tmul p (a,b,c) = (p*a, p*b, p*c)  -- multiply helper function

-- This is complicated, the list is the possible outcomes, probability mass weighted
sabotage' :: Int -> [Outcome]
sabotage' n = [  tmul (hits !! 0) loss,
                 tmul (hits !! 1) dmg,
                 tmul (hits !! 2) (win 2),
                 tmul (hits !! 3) (win 3),
                 tmul (hits !! 4) (win 4),
                 tmul (hits !! 5) (win 5),
                 tmul (hits !! 6) (win 6)
              ]
              where loss' = predict $ sabotageLost n    -- If sabotage is lost, what's the prediction
                    dmg'  = predict $ sabotageDmg  n    -- If sabotage damages, what's the prediction
                    win'  = predict $ sabotageWon  n    -- If sabotage wins, what's the prediction
                    loss  = addsurv loss' ((fromIntegral n)*(-0.5)) -- All committed fighters lost
                    dmg   = addsurv dmg'  (fighterValue n 1 (survival dmg')) -- one survives
                    win s = addsurv win'  (fighterValue n s (survival win')) -- s survive
                    fighterValue n s ps = (-0.5)*(ps*(fromIntegral (n-s))+(1-ps)*(fromIntegral n))
                    hits  = sabotageRunHitProbabilityMass n -- Probability of hits in sabotage run
                    survival (_, s, _) = s            -- helper function to extract survival prob
                    addsurv (w,s,v) v2 = (w, s, v+v2) -- Add value of fighters back
 
-- Add up the 6 possible outcomes
sabotage n = foldl (\(a,b,c) (d,e,f) -> (a+d, b+e, c+f)) (0.0, 0.0, 0.0) (sabotage' n)