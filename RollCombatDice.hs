module RollCombatDice where

import Fleet
import Binomial
import Data.List

type Probabilities = ([Probability], [Probability])

  -----------------------------------------------------------------
 --
-- Helper pair functions
--
papp f (a,b)      = (f a, f b)
pmap f (a,b)      = (map f a, map f b)
pzip (a,b) (c,d)  = (zip a c, zip b d)

-- Compute number of units from state (i.e. get rid of hit tracking)
size :: Fleets -> ([Int], [Int])
size = pmap snd

-- Extract units present from fleets
units :: Fleets -> ([Unit], [Unit])
units = pmap fst

-- Number of dice each unit gets
dice :: Fleets -> ([Int], [Int])
dice fleets = pmap rolls (units fleets)
--
---- Compute number of throws for each fleet unit
throws :: Fleets -> ([Int], [Int])
throws fleets = papp (map (\(x,y) -> x*y)) $ pzip (size fleets) (dice fleets)

---- normalization factor to handle null case
nullFactor :: [Probability] -> [Probability] -> Probability
nullFactor a b = 1/(1-(head a)*(head b))

---- Is a state terminal? i.e. one-side has reached zero units
terminal :: Fleets -> Bool
terminal ([], _ ) = True
terminal (_ , []) = True
terminal _        = False

-- Turn a pair of fleets into a pair of probabilities to hit
hitProb :: Fleets -> Probabilities
hitProb = pmap toHit.units

-- Turn a pair of fleets into the bivariate probability mass function list
-- in number of hits order
hitMass :: Fleets -> Probabilities
hitMass fleets = papp dmultinomial $ pzip (throws fleets) (hitProb fleets)

-- remove Casualties for a side
removeCasualties :: Fleet -> Int -> Fleet
removeCasualties [] _  = []
removeCasualties f  0  = f
removeCasualties f n = sort $ removeCasualties' (sort f) n

removeCasualties' :: Fleet -> Int -> Fleet
removeCasualties' ((unit,count):fs) n = 
    case sustainDamage unit of
        Just s  ->  if n < count
                        then (unit, count - n):((s, count-n):fs)
                        else if n > count
                               then removeCasualties ((s, count):fs) (n - count)
                               else sort $ (s, count):fs
        Nothing ->  if n < count
                        then (unit, count - n):fs
                        else if n > count
                                then removeCasualties fs (n-count)
                                else fs

-- Value of a fleet
value :: Fleet -> Double
value f = sum $ map (\(u, n) -> (fromIntegral n)*(cost u))  f

-- Value of a change in position relative to the first fleet
valueDiff :: Fleets -> Fleets -> Double
valueDiff (bus, bthem) (aus, athem) =  (value aus) - (value bus) + (value bthem) - (value athem)

  -----------------------------------------------------------------
 -- Now to build the list of probable outcomes from a state
--
outcomes :: Fleets -> [(Probability, Fleets)]
outcomes (us, them) = tail cross  where
        (hus, hthem) = hitMass (us, them) -- Inflicted hits
        n        = nullFactor hus hthem
        oa       = zip hthem $ map (removeCasualties us)   [0..((length hthem)-1)]
        ob       = zip hus   $ map (removeCasualties them) [0..((length hus)-1)]
        cross    = [(n*(fst a)*(fst b), (snd a, snd b)) | a <- oa, b <- ob]
