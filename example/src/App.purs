module App  where

import MultipleSelect (Event, State, foldp, initialState, view) as MS
import MultipleSelect.Dom (DOM)

import Prelude (discard, ($), (#))
import Data.List (List(..), (:))
import Pux.DOM.HTML (HTML, child)
import Text.Smolder.HTML (div, h1)
import Text.Smolder.Markup (Attribute, text, (!))
import Pux (EffModel, noEffects, mapEffects, mapState)

import Pux.DOM.HTML.Attributes (style)
import CSS.TextAlign (textAlign, center)

data Event
  = NoOp
  | InstrumentEvent MS.Event

type State = {
  instrumentChoices :: MS.State
}

instruments :: List String
instruments =
    "acoustic_grand_piano"
  : "cello"
  : "harpsichord"
  : "marimba"
  : "trombone"
  : "trumpet"
  : "vibraphone"
  : "viola"
  : "violin"
  : Nil

initialState :: State
initialState =
  {
    instrumentChoices : MS.initialState "add an instrument" instruments
  }

foldp :: ∀ fx. Event -> State -> EffModel State Event (dom :: DOM | fx)
foldp NoOp state =  noEffects $ state
foldp (InstrumentEvent e) state =
  MS.foldp e state.instrumentChoices
    # mapEffects InstrumentEvent
    # mapState \inst -> state { instrumentChoices = inst }


viewInstrumentSelect :: State -> HTML Event
viewInstrumentSelect state =
  child InstrumentEvent MS.view $ state.instrumentChoices

view :: State -> HTML Event
view state =
    div $ do
      h1 ! centreStyle $ text "Test change instruments"
      viewInstrumentSelect state

centreStyle :: Attribute
centreStyle =
  style do
    textAlign center
