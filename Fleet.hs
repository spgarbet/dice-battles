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
            DamagedWarSun      |
            PDS                | MagenPDS        | GravitonPDS | GravitonMagenPDS |
            ShockTroop         | GroundForce     | 
            SpaceDock
                deriving (Show, Eq, Enum, Ord)

-- Make a Unit memoizable, to speed later computations
instance Bounded Unit where
    minBound = Dreadnought
    maxBound = SpaceDock
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
cost PDS                = 1
cost MagenPDS           = 1
cost GravitonPDS        = 1
cost GravitonMagenPDS   = 1
cost GroundForce        = 0.5
cost ShockTroop         = 1

-- Number of rolls of battle dice for a given unit during space combat
type Rolls = Int

rolls :: Unit -> Rolls
rolls WarSun           = 3
rolls DamagedWarSun    = 3
rolls PDS              = 0
rolls MagenPDS         = 0
rolls GravitonPDS      = 0
rolls GravitonMagenPDS = 0
rolls GroundForce      = 0
rolls ShockTroop       = 0
rolls _                = 1

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
battleDice PDS                = 6
battleDice MagenPDS           = 5
battleDice GravitonPDS        = 6  -- But it rerolls failures !!
battleDice GravitonMagenPDS   = 5  -- Ditto

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


     