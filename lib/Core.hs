module Core where

type Name = String
type Initiative = Int
type Round = Int
data Allegiance = Friend | Foe deriving Eq
data Combatant = Combatant { name :: Name
                           , initiative :: Initiative
                           , allegiance :: Allegiance
                           }
                 deriving Eq

instance Show Combatant where
  show c = (name c) ++ " (" ++ show (initiative c) ++ ")" 

newtype CombatOrder = CombatOrder [(Int,[Combatant])]
  deriving Show
newtype Combat = Combat [Combatant]

theHeroes :: [Combatant]
theHeroes = [ Combatant "Hassan" 7 Friend
            , Combatant "Nolleman" 9 Friend
            , Combatant "Johann" 13 Friend
            ]

isFriend :: Combatant -> Bool
isFriend c = allegiance c == Friend

mkCombat :: [Combatant] -> Combat
mkCombat heroes = Combat heroes

addFoes :: Combat -> Combatant -> Int -> Combat
addFoes (Combat combatants) foe count = Combat combatants'
  where combatants' = combatants ++ replicate count foe

infix 2 <|>
(<|>) :: Int -> Int -> Bool
a <|> b = b `mod` a == 0


getCombatOrder :: Combat -> Round -> Int -> CombatOrder
getCombatOrder (Combat combatants) round l =
  CombatOrder
  $ take l
  $ filter (\(_,cs) -> length cs > 0)
  $ fmap assignCombatants rounds
  where rounds = [round..]
        assignCombatants :: Int -> (Int,[Combatant])
        assignCombatants i = (i, filter (\c -> initiative c <|> i) combatants)

