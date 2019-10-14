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
main = startGUI defaultConfig { jsStatic = Just ".", jsAddr = Nothing } setup

setup :: Window -> UI ()
setup window = do
  UI.addStyleSheet window "bootstrap.min.css"
  UI.addStyleSheet window "main.css"
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
      
      UI.div #. "row" #+ [ UI.span #. "col" # set UI.text (show c)
                         , element bdelete #. "col" ]
      
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
      
      UI.div #. "container" #+
        ([ UI.div #. "row" #+ [ element tname #. "col", element tinitiative #. "col", element bsubmit #. "col" ]
        , UI.hr ]
        ++ combatantSection ++
        [ UI.hr
        , UI.div #. "row" #+ [ element bswitchcombat #. "col", element bnextround #. "col", element sround #. "col" ] ]
        ++ combatOrderSection)

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
