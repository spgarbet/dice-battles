import Data.List

  -----------------------------------------------------------------
 -- Binomial definitions
--
-- Binomial coefficient
choose :: Int -> Int -> Int
choose n 0 = 1
choose 0 k = 0
choose n k = (choose (n-1) (k-1)) * n `div` k

-- Binomial distribution
dbinom :: Int -> Int -> Double -> Double
dbinom k n p = binomCoef*(p^k')*((1-p)^(n'-k'))
               where binomCoef = fromIntegral (choose n k)
                     k'        = fromIntegral k
                     n'        = fromIntegral n

  -----------------------------------------------------------------
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


-- Compute the sum of the products of two lists of numbers
cross a b = sum $ map (\(x,y) -> x*y) $ zip a b

-- hit odds for a given set of dice
-- example of a war sun and 6 fighters:
--   odds [(3, 0.8), (6, 0.2)]
odds :: [(Int, Double)] -> [Double]
odds [] = []
odds ((dice,prob):[]) =  map (\x -> dbinom x dice prob) [0..dice]
odds ((dice,prob):xs) = convolute (map (\x -> dbinom x dice prob) [0..dice]) (odds xs)

  -----------------------------------------------------------------
 -- Data structure examples
--
-- The pair represents the two sides of a conflict
-- Each side has the respective data for the list of units
-- The list is also the preferred order to take hits
exampleProb  = ([0.8],   [0.2]) :: ([Double], [Double])
exampleDice  = ([3],     [1])   :: ([Int],    [Int])
-- State is the number of units of a given type, note the first one is a war-sun and
-- I'm using a list to denote it's progression down through hits.
exampleState = ([[1,0]], [[1]]) :: ([[Int]],  [[Int]])

  -----------------------------------------------------------------
 -- Now to deal with state pairs 
--
-- Helper pair functions
pmap f (a,b)     = (f a, f b)
pzip (a,b) (c,d) = (zip a c, zip b d)

-- Compute number of units from state
units   state     = pmap (map sum) state
-- Compute number of dice from state and dice per unit
dice    state d   = pmap (map (\(x,y) -> x*y)) $ pzip (units state) d
-- compute probable hits per side
hitProb state d p = pmap odds $ pzip (dice state d) p

-- normalization factor to handle null case
nullFactor a b = 1/(1-(head a)*(head b))

-- Is a state terminal? i.e. one-side has reached zero
terminal state = (a==0) || (b==0) where
                 (a,b) = pmap sum $ units state

  -----------------------------------------------------------------
 -- Important assumptions about preferences to casualties
--
-- For the multi-hits, knock down to bottom category
multiHit h (x:[]) = (h, [x])
multiHit h (x:xs) = if x <= h
                      then multiHit (h-x) $ (x'+x):t
                      else (0, x-h : x'+h : t)
                    where x' = head xs
                          t  = tail xs

-- When no multi-hit units are left, go for the singles
singles h x = singles' h $ map head x where
                singles' h x = map (\x -> [x]) $ snd $ mapAccumL f h x
                f acc u = if acc > u then (acc-u, 0) else (0, u-acc)

--apply hits to a state
damage side hits = if h > 0 then singles h s else s where (h, s) = mapAccumL multiHit hits side

--example : damage [[10], [2,1]] 5

  -----------------------------------------------------------------
 -- Now to build the list of probable outcomes from a state
--
outcomes s d p = map f g where
                 (ha, hb) = hitProb s d p
                 n = nullFactor ha hb
                 ob = zip ha $ map (damage (snd s)) [0..((length ha)-1)]
                 oa = zip hb $ map (damage (fst s)) [0..((length ha)-1)]
                 o = sort $ tail [ (((snd a, snd b)), n*(fst a)*(fst b) ) | a <- oa, b <- ob]
                 g = groupBy (\x y -> (fst x) == (fst y)) o
                 f x = (sum (map snd x), fst (head x))

wins s d p = foldl f 0.0 o' where
             o = outcomes s d p
             o' = map (\(a,b) -> (pmap sum (units b), a, b)) o
             f total ((them, us), prob, state) = 
               if them == 0
                 then total + prob
                 else if us > 0
                        then
                          total+prob*(wins state d p)
                        else total

-- n fighters versus a single war sun
fighters n = 100 * (wins ([[1,0]],[[n]]) ([3],[1]) ([0.8],[0.2])) 

-- 1 carrier with 6 fighters vs. War sun, 4 fighters, 2 cruisers
-- What happens, when n fighters leave for sabotage run, and war sun survives
sabotage_run_failure n = 100 * (wins ([[4],[2],[1,0]],[[6-n],[1]]) ([1,1,3],[1,1]) ([0.2, 0.3, 0.8],[0.2,0.2])) 

sabotage_run_success n = 100 * (wins ([[4],[2]],[[6-n],[1]]) ([1,1],[1,1]) ([0.2, 0.3],[0.2,0.2])) 

-- > failure_odds <- c(8.332132827700946e-2, 9.539364917006783e-3, 5.286671171755559e-4, 1.0371667262687312e-5, 3.794170439987539e-8, 3.86263808420207e-12, 2.3403697656337865e-22)
-- > 
-- > success_odds <- c(8.332132827700946e-2, 28.22842074816851, 13.947608655178446, 4.536908589124441, 0.788230992032397, 4.729588858580182e-2, 2.1031956630313879e-4)
-- > 1-pbinom(0, 0:6, 0.02)
-- [1] 0.00000000 0.02000000 0.03960000 0.05880800 0.07763184 0.09607920 0.11415762
-- > pbinom(0, 0:6, 0.02)*failure_odds + (1-pbinom(0, 0:6, 0.02))*success_odds
-- [1] 8.332133e-02 5.739170e-01 5.528330e-01 2.668163e-01 6.119186e-02 4.544151e-03 2.400958e-05
-- >
