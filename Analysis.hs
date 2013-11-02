import Fleet
import SpaceBattle

x = ([ (WarSun, 2), (Cruiser, 3)], [(Fighter, 5), (Carrier, 1),(CyberFighter, 2)]) :: (Fleet, Fleet)  

simple =  ([(Fighter, 5)], [(WarSun, 1)] ) :: (Fleet, Fleet)  

fighter n = predict d where
             d = ([(Fighter, n)], [(WarSun, 1)] ) :: (Fleet, Fleet)

fighterVsWarSun = map fighter [1..10]