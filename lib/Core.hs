module Core where

type Name = String
type Initiative = Int
type Round = Int
data Combatant = Combatant { name :: Name
                           , initiative :: Initiative
                           }

instance Show Combatant where
  show c = show $ name c

newtype CombatOrder = CombatOrder [(Int,[Combatant])]
  deriving Show
newtype Combat = Combat [Combatant]

theHeroes :: [Combatant]
theHeroes = [ Combatant "Nolleman" 5
            , Combatant "Hassan" 7
            , Combatant "Johann" 14
            ]

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
  $ filter (\(_,cs) -> length cs > 0)
  $ fmap assignCombatants rounds
  where rounds = [round..round+l-1]
        assignCombatants :: Int -> (Int,[Combatant])
        assignCombatants i = (i, filter (\c -> initiative c <|> i) combatants)

