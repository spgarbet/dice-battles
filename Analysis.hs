import Fleet
import SpaceBattle
import Binomial
import Text.Show.Pretty

  -----------------------------------------------------------------
 -- Simple example of pitting fighters versus a War Sun
--
fighter n = predict ([(Fighter, n)], [(WarSun, 1)] )
fighterVsWarSun = putStr $ ppShow $ map fighter [1..10]

-- Warning: above about 60 units on a side, the round-off error destroys the validity of any results

secondExample n = predict ([(Fighter, n), (Carrier, 1)], [(WarSun, 1), (Cruiser, 1)])
second = putStr $ ppShow $ map secondExample [1..10]

sec = putStr $ ppShow $ map (\(a,b,c,d) -> (100.0*a, 100.0*b, 100.0*c, d)) $ map secondExample [1..10]