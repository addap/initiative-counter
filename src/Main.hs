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
  bsubmit <- UI.button # set UI.text "+"
  tname <- UI.input
  tinitiative <- UI.input
  bswitchcombat <- UI.button
  bendcombat <- UI.button # set UI.text "End Combat"
  bnextround <- UI.button # set UI.text "Next Round"
  sround <- UI.span
  dCombatOrderSection <- UI.div
  dCombatantSection <- UI.div

  (deleteEvt, deleteHdl) <- liftIO $ (newEvent :: IO (Event (Combatant, Element), Handler (Combatant, Element)))
  liftIO $ register deleteEvt (\(n, _) -> putStrLn (show n))

    -- behaviors & events
  let eChangeMode = UI.click bswitchcombat
      eNextRound = UI.click bnextround
  -- make behaviors out of name and initiative
  bName <- stepper "" $ UI.valueChange tname
  bInitiative <- stepper 0 $ fromMaybe 1 . (readMaybe :: String -> Maybe Int) <$> UI.valueChange tinitiative
  -- behavior that tracks if we are in fight or normal mode
  bMode <- accumB Normal $ (const switchMode) <$> eChangeMode
  -- filter the cases where bMode == Fight since bMode is only changed slightly after the eChangeMode event happens
  let eEndCombat = filterE (==Fight) (bMode <@ eChangeMode)
      eResetRound = const 1 <$> eEndCombat
      eRemoveFoes = const (filter (\c -> allegiance c == Friend)) <$> eEndCombat
  
  -- behavior that contains the current "new combatant" i.e. depending on what the name and initiative fields contain and in which mode we are now
  let bNewCombatant = Combatant <$> bName <*> bInitiative <*> (modeToAllegiance <$> bMode)
      -- event that contains the new combatant by attaching the current state of the behavior to each click on bsubmit
      eNewCombatant = bNewCombatant <@ UI.click bsubmit
      -- map cons over the event to turn it into a function that adds a new combatant
      eAddCombatant = (:) <$> eNewCombatant
      eDeleteCombatant = L.delete . fst <$> deleteEvt
      eChangeCombatants = eAddCombatant <=> eDeleteCombatant <=> eRemoveFoes
  -- behavior that contains the current combatants. changed by eChangeCombatants
  bCombatants <- accumB [] eChangeCombatants

  let eAddCombatantElement = combatantElement deleteHdl <$> eNewCombatant
  -- register to event to add ui elements
  liftIO $ register eAddCombatantElement $ \e -> runUI window (element dCombatantSection #+ [e]) >> return ()
  -- delete combatants when their '-' button is pressed
  liftIO $ register deleteEvt $ runUI window . delete . snd
  -- instead of registering both functions and using runUI I could also make a behavior for both events and use the onChange event handler from that to do the same thing.

  (eStepRound, hStepRound) <- liftIO $ (newEvent :: IO (Event Round, Handler Round))
  bRound <- stepper 1 $ unionWith const eStepRound eResetRound
  -- bahvior for the current combat order
  -- let bCombatOrder :: UI (Behavior CombatOrder)
  let bCombatOrder = getCombatOrderFrom <$> bRound <*> bCombatants
      -- bRound :: UI (Behavior Round)
      -- bNextRound :: UI (Behavior Round)
      bNextRound = fmap (\(CombatOrder co) -> fst $ co !! 1) bCombatOrder
  -- pretty ugly but it allows me to notify the stepper of bRound that it should step to the next value which is currently in bRound
  -- FIXME is there a better way to do recursive Behaviors?
  liftIO $ register eNextRound $ const $ runUI window (currentValue bNextRound) >>= hStepRound

  onChanges bCombatOrder (\(CombatOrder co) -> do
                             elm <- UI.ul #+ map toCombatElement (take 10 co)
                             element dCombatOrderSection # set children [ elm ])

  element sround # sink UI.text (("Current Round: " ++) . show <$> bRound)
  element bswitchcombat # sink UI.text (modeButton <$> bMode)
  element dCombatOrderSection # sink display (modeDisplay <$> bMode)
  
  let content =
        [ UI.div # brow #+ [ element tname # bcol 5, element tinitiative # bcol 5, element bsubmit # bcol 2 ]
        , UI.hr ]
        ++ [ element dCombatantSection ] ++
        [ UI.hr
        , UI.div # brow #+ [ element bswitchcombat # bcol 4, element bnextround # bcol 4, element sround # bcol 4 ] ]
        ++ [ element dCombatOrderSection ]
  
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


combatantElement :: Handler (Combatant, Element) -> Combatant -> UI Element
combatantElement hdl c = do
  bdelete <- UI.button # set UI.text "-"
  elm <- UI.div # brow #+ [ UI.span # bcol 10 # set UI.text (show c)
                             , element bdelete # bcol 2 ]
  liftIO $ register (UI.click bdelete) $ \() -> hdl (c, elm)
  return elm


-- bootstrap functions
brow :: UI Element -> UI Element
brow e = e #. "row"

bcol :: Int -> UI Element -> UI Element
bcol n e = e #. ("col-" ++ show n)
