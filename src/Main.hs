module Main where

{-# LANGUAGE ScopedTypeVariables #-}

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


import Data.IORef
import Control.Monad (void)
import qualified Data.List as L (delete)
import Data.Bool (bool)
import Data.List (intersperse)

import Core

combatOrderLength = 10
  
main :: IO ()
main = startGUI defaultConfig { jsStatic = Just "static", jsAddr = Nothing, jsPort = Nothing } setup

setup :: Window -> UI ()
setup window = do
  UI.addStyleSheet window "bootstrap.min.css"
  UI.addStyleSheet window "main.css"

  -- https://stackoverflow.com/questions/36813919/bootstrap-is-not-working-on-mobile
  meta <- UI.meta # set UI.content "width=device-width, initial-scale=1" # set UI.name "viewport"
  getHead window #+ [ element meta ]
  
  return window # set UI.title "Initiative Counter"

  -- active elements
  bsubmit <- UI.button # set UI.text "+"
  tname <- UI.input
  tinitiative <- UI.input
  bswitchcombat <- UI.button
  -- bendcombat <- UI.button # set UI.text "End Combat"
  bnextround <- UI.button # set UI.text "Next Round"
  sround <- UI.span
  
  -- state
  -- combatants :: IORef [Combatant]
  combatants <- liftIO $ newIORef theHeroes
  -- round :: IORef Int
  roundRef <- liftIO $ newIORef 1
  nextRoundRef <- liftIO $ newIORef 1
  -- combat :: IORef Bool
  combat <- liftIO $ newIORef False
  
  -- functionality
  let
    combatantElement :: Combatant -> UI Element
    combatantElement c = do
      bdelete <- UI.button # set UI.text "-"
      on UI.click bdelete $ \_ -> removeCombatant c >> redoLayout
      
      UI.div # brow #+ [ UI.span # bcol 10 # set UI.text (show c)
                         , element bdelete # bcol 2 ]
      
    redoLayout :: UI ()
    redoLayout = void $ do
      layout <- mkLayout =<< liftIO (readIORef combatants)
      getBody window # set children [ layout ]

    mkLayout :: [Combatant] -> UI Element
    mkLayout cs = do
      let combatantSection = map combatantElement cs
      
      inCombat <- liftIO $ readIORef combat
      set UI.text (bool "Start Combat" "End Combat" inCombat) $ return bswitchcombat
      
      round <- liftIO $ readIORef roundRef
      -- this lifts the functions instead of reading out the values of the IORef but it not really more legible
      -- combat <- liftIO $ liftM Combat (readIORef combatants)
      -- CombatOrder combatOrder <- liftIO $ liftM3 getCombatOrder (liftM Combat (readIORef combatants)) (readIORef round) (readIORef combatOrderLengthRef)
      let combat = Combat cs
          CombatOrder combatOrder = getCombatOrder combat round combatOrderLength
          combatOrderSection = if not inCombat then []
                               else [ UI.hr
                                    , UI.ul #+ map toCombatElement combatOrder
                                    ]
          nextRound = fst $ combatOrder !! 1

      element sround # set UI.text ("Current Round: " ++ (show round))
      liftIO $ modifyIORef nextRoundRef (const nextRound)

      let content =
            [ UI.div # brow #+ [ element tname # bcol 5, element tinitiative # bcol 5, element bsubmit # bcol 2 ]
            , UI.hr ]
            ++ combatantSection ++
            [ UI.hr
            , UI.div # brow #+ [ element bswitchcombat # bcol 4, element bnextround # bcol 4, element sround # bcol 4 ] ]
            ++ combatOrderSection
                              
      UI.div #. "container-fluid" #+
        [ UI.div # brow #+
          [ UI.div # bcol 12 #+
            [ UI.div #. "jumbotron" #+ content ]
          ]
        ]

    toCombatElement :: (Int,[Combatant]) -> UI Element
    toCombatElement (i,cs) = UI.li #+ [ UI.span # set UI.text (show i ++ ": ")
                                      , UI.span #+ spans ]
      where spans = map (\c -> UI.span #. allegianceCSS (allegiance c) # set UI.text (name c)) cs

    allegianceCSS :: Allegiance -> String
    allegianceCSS Friend = "friend"
    allegianceCSS Foe = "foe"

    addCombatant :: UI ()
    addCombatant = do
      n <- get UI.value tname
      si <- get UI.value tinitiative
      inCombat <- liftIO $ readIORef combat
      let i = (read si :: Int)
          combatant = Combatant { name = n, initiative = i, allegiance = bool Friend Foe inCombat }
          
      liftIO $ modifyIORef combatants (combatant:)
      
    removeCombatant :: Combatant -> UI ()
    removeCombatant c = liftIO $ modifyIORef combatants (L.delete c)

    toNextRound :: UI ()
    toNextRound = do
      nextRound <- liftIO $ readIORef nextRoundRef
      liftIO $ modifyIORef roundRef (const nextRound)

    switchCombat :: UI ()
    switchCombat = do
      inCombat <- liftIO $ readIORef combat
      if inCombat
        then liftIO $ modifyIORef combatants (filter isFriend)
        else return ()
      liftIO $ modifyIORef combat not

  on UI.click bsubmit $ \_ -> addCombatant >> redoLayout
  on UI.click bnextround $ \_ -> toNextRound >> redoLayout
  on UI.click bswitchcombat $ \_ -> switchCombat >> redoLayout
  
  redoLayout



-- bootsrap functions
brow :: UI Element -> UI Element
brow e = e #. "row"

bcol :: Int -> UI Element -> UI Element
bcol n e = e #. ("col-" ++ show n)
