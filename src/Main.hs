module Main where

{-# LANGUAGE ScopedTypeVariables #-}

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


import Control.Monad (void, liftM, liftM2)
import qualified Data.List as L (delete)
import Data.Bool (bool)
import Data.List (intersperse)
import Data.Bifunctor
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import Core

data Mode = Fight | Normal deriving (Eq, Show)

modeBool :: Mode -> Bool
modeBool Fight = True
modeBool Normal = False

switchMode :: Mode -> Mode
switchMode Fight = Normal
switchMode Normal = Fight

modeToAllegiance :: Mode -> Allegiance
modeToAllegiance Fight = Foe
modeToAllegiance Normal = Friend

modeButton :: Mode -> String
modeButton Fight = "End Combat"
modeButton Normal = "Start Combat"

modeDisplay :: Mode -> String
modeDisplay Fight = "block"
modeDisplay Normal = "none"

combatOrderLength = 10
nameless = "The Unspeakable Horror"

-- unionWith that always takes the first event
infixl 4 <=>
(<=>) = unionWith const
  
main :: IO ()
main = startGUI defaultConfig { jsStatic = Just "static", jsAddr = Nothing, jsPort = Nothing } setup

setup :: Window -> UI ()
setup window = void $ do
  UI.addStyleSheet window "bootstrap.min.css"
  UI.addStyleSheet window "main.css"

  -- https://stackoverflow.com/questions/36813919/bootstrap-is-not-working-on-mobile
  meta <- UI.meta # set UI.content "width=device-width, initial-scale=1" # set UI.name "viewport"
  getHead window #+ [ element meta ]
  
  window # set' UI.title "Initiative Counter"

  -- display property to hide elements
  let display :: Attr Element String
      display = fromObjectProperty "style.display"

    -- active elements
  btnSubmit <- UI.button # set UI.text "+"
  iptName <- UI.input
  iptInitiative <- UI.input
  btnEndCombat <- UI.button # set UI.text "End Combat"  # set display "none"
  btnStartCombat <- UI.button # set UI.text "Start Combat"
  btnNextRound <- UI.button # set UI.text "Next Round"
  spRound <- UI.span
  dCombatOrderSection <- UI.div
  dCombatantSection <- UI.div

    -- behaviors & events
  bName <- stepper nameless $ (\s -> if s == "" then nameless else s) <$> UI.valueChange iptName
  bInitiative <- stepper 1 $ fromMaybe 1 . (readMaybe :: String -> Maybe Int) <$> UI.valueChange iptInitiative
  -- events for entering and exiting combat mode
  let eNextRound = UI.click btnNextRound
      eStartCombat = const Fight <$> UI.click btnStartCombat
      eEndCombat = const Normal <$> UI.click btnEndCombat
      eResetRound = const 1 <$> eEndCombat
      eRemoveFoes = const (filter (\c -> allegiance c == Friend)) <$> eEndCombat
  bMode <- stepper Normal $ eStartCombat <=> eEndCombat
  
  -- custom event to handle pressing the '-' button on combatants
  (evtDelete, hdlDelete) <- liftIO $ (newEvent :: IO (Event Combatant, Handler Combatant))
  -- behavior that contains the current "new combatant" i.e. depending on what the name and initiative fields contain and in which mode we are now
  let bNewCombatant = Combatant <$> bName <*> bInitiative <*> (modeToAllegiance <$> bMode)
      -- event that contains the new combatant by attaching the current state of the behavior to each click on bsubmit
      eNewCombatant = bNewCombatant <@ UI.click btnSubmit
      -- map cons over the event to turn it into a function that adds a new combatant
      eAddCombatant = (:) <$> eNewCombatant
      eDeleteCombatant = L.delete  <$> evtDelete
      eChangeCombatants = eAddCombatant <=> eDeleteCombatant <=> eRemoveFoes
  -- behavior that contains the current combatants
  bCombatants <- accumB [] eChangeCombatants

  onChanges bCombatants (\cs -> do
                            elm <- UI.ul #+ (combatantElement hdlDelete <$> cs)
                            element dCombatantSection # set children [ elm ])

  -- custom event to trigger updating of the round behavior
  -- found no other way since bRound-bCombatOrder-bNextRound are recursive
  (evtStepRound, hdlStepRound) <- liftIO $ (newEvent :: IO (Event Round, Handler Round))
  bRound <- stepper 1 $ evtStepRound <=> eResetRound
  -- behavior for the current combat order
  let bCombatOrder = getCombatOrderFrom <$> bRound <*> bCombatants
      bNextRound = fmap (\(CombatOrder co) -> fst $ co !! 1) bCombatOrder
  -- pretty ugly but it allows me to notify the stepper of bRound that it should step to the next value which is currently in bNextRound
  -- FIXME is there a better way to do recursive Behaviors?
  liftIO $ register eNextRound $ const $ runUI window (currentValue bNextRound) >>= hdlStepRound

  onChanges bCombatOrder (\(CombatOrder co) -> do
                             elm <- UI.ul #+ (toCombatElement <$> (take combatOrderLength co))
                             element dCombatOrderSection # set children [ elm ])

  element spRound # sink UI.text (("Current Round: " ++) . show <$> bRound)
  element dCombatOrderSection # sink display (modeDisplay <$> bMode)
  element btnStartCombat # sink display (modeDisplay . switchMode <$> bMode)
  element btnEndCombat # sink display (modeDisplay <$> bMode)
  
  let content =
        [ UI.div # brow #+ [ element iptName # bcol 5, element iptInitiative # bcol 5, element btnSubmit # bcol 2 ]
        , UI.hr
        , element dCombatantSection
        , UI.hr
        , UI.div # brow #+ [ element btnStartCombat # bcol 4, element btnEndCombat # bcol 4, element btnNextRound # bcol 4, element spRound # bcol 4 ]
        , element dCombatOrderSection ]
  
      layout =  UI.div #. "container-fluid" #+
                [ UI.div # brow #+
                  [ UI.div # bcol 12 #+
                    [ UI.div #. "jumbotron" #+ content ]
                  ]
                ]

  getBody window #+ [ layout ]



toCombatElement :: (Int,[Combatant]) -> UI Element
toCombatElement (i,cs) = UI.li #+ [ UI.span # set UI.text (show i ++ ": ")
                                      , UI.span #+ spans ]
  where spans = map (\c -> UI.span #. allegianceCSS (allegiance c) # set UI.text (name c)) cs

allegianceCSS :: Allegiance -> String
allegianceCSS Friend = "friend"
allegianceCSS Foe = "foe"


combatantElement :: Handler Combatant -> Combatant -> UI Element
combatantElement hdl c = do
  bdelete <- UI.button # set UI.text "-"
  liftIO $ register (UI.click bdelete) $ const $ hdl c
  UI.div # brow #+ [ UI.span # bcol 10 # set UI.text (show c)
                   , element bdelete # bcol 2 ]


-- bootstrap functions
brow :: UI Element -> UI Element
brow e = e #. "row"

bcol :: Int -> UI Element -> UI Element
bcol n e = e #. ("col-" ++ show n)
