module Fleet where

import Data.Function.Memoize
import Data.Maybe

  -----------------------------------------------------------------
 --
-- Basic Type Definition for Units in a Players Fleet
--
-- Given in remove casualty ordering (lowest value to highest value for a hit)
data Unit = Dreadnought        | WarSun          |
            Fighter            | CyberFighter    | AdvFighter | AdvCyberFighter |
            Destroyer          | HylarVDestroyer | 
            Cruiser            | HylarVCruiser   |
            Carrier            |
            DamagedDreadnought |
            DamagedWarSun 
                deriving (Show, Eq, Enum, Ord)

-- Make a Unit memoizable, to speed later computations
instance Bounded Unit where
    minBound = Dreadnought
    maxBound = DamagedWarSun
instance Memoizable Unit where
    memoize = memoizeFinite

  -----------------------------------------------------------------
 --
--  This is the essential definition of information on a race card
--
-- Base resource cost of a unit in TI3
type Cost = Double

cost :: Unit -> Cost
cost Dreadnought        = 5
cost Carrier            = 3
cost Cruiser            = 2
cost CyberFighter       = 0.5
cost AdvFighter         = 0.5
cost AdvCyberFighter    = 0.5
cost Destroyer          = 1
cost HylarVCruiser      = 2
cost HylarVDestroyer    = 1
cost Fighter            = 0.5
cost WarSun             = 12
cost DamagedWarSun      = 12
cost DamagedDreadnought = 5

-- Number of battle dice for a given unit
type Rolls = Int

rolls :: Unit -> Rolls
rolls WarSun        = 3
rolls DamagedWarSun = 3
rolls _             = 1

-- BattleDice
type BattleDice = Int

battleDice :: Unit -> BattleDice
battleDice Dreadnought        = 5
battleDice Carrier            = 9
battleDice Cruiser            = 7
battleDice Destroyer          = 9
battleDice HylarVCruiser      = 6
battleDice HylarVDestroyer    = 8
battleDice Fighter            = 9
battleDice CyberFighter       = 8
battleDice AdvFighter         = 8
battleDice AdvCyberFighter    = 7
battleDice WarSun             = 3
battleDice DamagedWarSun      = 3
battleDice DamagedDreadnought = 3

-- This returns ranks of sustain damage possible damage
sustainDamage :: Unit -> Maybe Unit
sustainDamage Dreadnought = Just DamagedDreadnought
sustainDamage WarSun      = Just DamagedWarSun
sustainDamage _           = Nothing

-- Is a unit capable of Anti-fighter Barrage
-- i.e. before battle 2 dice for each destroyer, every hit eliminates a Fighter
barrage :: Unit -> Bool
barrage Destroyer       = True
barrage HylarVDestroyer = True
barrage _               = False

-- Is a unit eligible for a sabotage run?
sabotage :: Unit -> Bool
sabotage Fighter         = True
sabotage CyberFighter    = True
sabotage AdvFighter      = True
sabotage AdvCyberFighter = True
sabotage _               = False

-- A Fleet of units
type Fleet = [(Unit, Int)]

type Fleets = (Fleet, Fleet)  -- Two sides fleet's


     