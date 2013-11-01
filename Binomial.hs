module Binomial
( Probability
, choose
, dbinom
, convolute
, dmultinomial
, toHit
) where

import Data.List
import Fleet
import Data.Function.Memoize
import Debug.Trace

-- A probability is a double from 0.0 to 1.0
type Probability = Double

  -----------------------------------------------------------------
 --
-- Probability of a unit scoring a hit on a single dice roll
--
toHit :: Unit -> Probability
toHit f = (fromIntegral (11 - (battleDice f))) / 10.0

  -----------------------------------------------------------------
 --
-- Binomial coefficient
--
choose' :: Int -> Int -> Int
choose' n 0 = 1
choose' 0 k = 0
choose' n k = (choose (n-1) (k-1)) * n `div` k

choose = memoize2 choose'

  -----------------------------------------------------------------
 -- 
-- Binomial distribution, p.m.f.
--  n is number of trials
--  p is the probability of success on a trial
--
-- Returned is a list of probabilities,
--    the list position is the number of hits at that probability
--
-- Example:
-- > dbinom 3 0.2
-- [0.5120000000000001,0.3840000000000001,9.600000000000003e-2,8.000000000000002e-3]
--
dbinom :: Int -> Probability -> [Probability]
dbinom n p =  map dbinom' (map fromIntegral [0..n]) where
                  dbinom' k = (fromIntegral (choose n k))*(p^k)*((1-p)^(n'-k))
                  n'        = fromIntegral n

-- Compute the sum of the products of two lists of numbers
cross a b = sum $ map (\(x,y) -> x*y) $ zip a b
                     
  -----------------------------------------------------------------
 --
-- Convolution to determine outcome possibilities
--
convolute a b = if (length a) <= (length b)
                  then convolute' a b
                  else convolute' b a

convolute' a b = map f [1..s] where
                   s    = la+lb-1
                   la   = length a
                   lb   = length b
                   f n = if n <= la
                           then cross (take n a) (reverse (take n b))
                           else cross (take t (drop da a)) (reverse (take t (drop db b)))
                           where
                                t    = if (s-n +1) > la then la else s-n+1
                                da   = if (n-lb) < 0 then 0 else n-lb
                                db   = n-la


  -----------------------------------------------------------------
 --
-- Compute the multinomial p.m.f.
-- i.e. probability of a given number of successes for a list of dice rolls
-- Input: List of number of rolls at a given probability
--
-- Returned is a list of probabilities,
--    the list position is the number of hits at that probability
--
-- Example:
-- > dmultinomial [(3, 0.8), (6, 0.2)]
-- [2.097152e-3,2.8311552000000004e-2,0.14037811200000005,0.3094609920000002,0.30368563200000026,0.15877324800000012,4.798924800000003e-2,8.460288000000009e-3,8.110080000000006e-4,3.276800000000003e-5]
-- > sum $ dmultinomial [(3, 0.8), (6, 0.2)]
-- 1.0000000000000007
--
dmultinomial :: [(Rolls, Probability)] -> [Probability]
dmultinomial [] = []
dmultinomial ((dice,prob):[]) = dbinom dice prob
dmultinomial ((dice,prob):xs) = convolute (dbinom dice prob) (dmultinomial xs)