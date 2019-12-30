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

nextRound :: CombatOrder -> CombatOrder
nextRound (CombatOrder co) = CombatOrder (tail co)

theHeroes :: [Combatant]
theHeroes = [ Combatant "Hassan" 7 Friend
            , Combatant "Nolleman" 9 Friend
            , Combatant "Johann" 13 Friend
            ]

isFriend :: Combatant -> Bool
isFriend c = allegiance c == Friend

addFoes :: [Combatant] -> Combatant -> Int -> [Combatant]
addFoes combatants foe count = combatants'
  where combatants' = combatants ++ replicate count foe

infix 2 <|>
(<|>) :: Int -> Int -> Bool
a <|> b = b `mod` a == 0

getCombatOrder :: [Combatant] -> CombatOrder
getCombatOrder = getCombatOrderFrom 0

getCombatOrderFrom :: Round -> [Combatant] -> CombatOrder
getCombatOrderFrom start combatants =
  CombatOrder
  $ filter (\(_,cs) -> length cs > 0)
  $ fmap assignCombatants rounds
  where rounds = [start..]
        assignCombatants :: Int -> (Int,[Combatant])
        assignCombatants i = (i, filter (\c -> initiative c <|> i) combatants)

