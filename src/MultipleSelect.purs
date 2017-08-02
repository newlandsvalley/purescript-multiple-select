module MultipleSelect (Event, State, foldp, initialState, view) where


import Prelude (bind, const, discard, pure, ($), (/=))
import Pux (EffModel, noEffects)
import Pux.DOM.Events (onClick, onChange, targetValue)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (a, div, option, select, span, ul, li)
import Text.Smolder.HTML.Attributes as At
import Text.Smolder.Markup (text, (!), (#!), (!?))
import Data.List (List(..), elem, filter, reverse, (:))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Control.Monad.Eff.Class (liftEff)

import Pux.DOM.HTML.Attributes (style)
import MultipleSelect.Dom (DOM, resetDefaultSelected)

-- | An implementation as a Pux module of an autonomus multiple select widget
-- | The state is initialised with an array of available menu options
-- | as and when they are selected, they are disabled from the available
-- | array and moved to the selected array.  The inverse operation is enabled
-- | by attaching a remove button to each selected option

data Event =
    AddSelection String
  | RemoveSelection String


type State = {
    instruction :: String          -- the instruction on what to select
  , available :: List String       -- available options
  , selected  :: List String       -- currently selected options
  }

initialState :: String -> List String -> State
initialState instruction available = {
    instruction : instruction
  , available : available
  , selected : Nil
  }

-- foreign import resetDefaultSelected :: forall eff. Unit -> Eff (dom :: DOM | eff) Unit
foldp :: ∀ fx. Event -> State -> EffModel State Event (dom :: DOM | fx)
foldp (AddSelection s) state =
  -- add the selection to the selected list and then ensure that the currently
  -- selected item in the list is reset to the default value (the disabled instruction)
  { state: state { selected = addSelection s state.selected }
    , effects:
       [
        do
          _ <- liftEff resetDefaultSelected
          pure Nothing
        ]
  }
foldp (RemoveSelection s) state =
  noEffects $ state { selected = removeSelection s state.selected }

-- | present the menu to add selections
addSelectionMenu :: State -> HTML Event
addSelectionMenu state =
  let
    isDisabled :: String -> Boolean
    isDisabled s =
      elem s state.selected
    f :: String -> HTML Event
    f s =
      div $ do
        (option !? (isDisabled s)) (At.disabled "disabled") $ text s
  in
    do
      select  ! At.id "selection-menu"
        #! onChange (\e -> AddSelection (targetValue e) )
          $ do
            option ! At.selected "selected" ! At.disabled "disabled" $ text state.instruction
            traverse_ f state.available

-- | add a selection to the end of the list
addSelection :: String -> List String -> List String
addSelection s ss =
  reverse $ s : (reverse ss)

-- | remove a selection from the list
removeSelection :: String -> List String -> List String
removeSelection s ss =
  filter ((/=) s) ss

-- | view the current selections
viewSelections :: State -> HTML Event
viewSelections state =
  let
    f :: String -> HTML Event
    f s =
      li ! At.className "msListItem" $ do
        span ! At.className "msListItemLabel" $ do
          text s
        a ! At.className "msListItemRemove" ! At.href "#" #! onClick (const $ RemoveSelection s) $ text "remove"
  in
    ul ! At.className "msList" $ do
      traverse_ f state.selected

view :: State -> HTML Event
view state =
  div $ do
    addSelectionMenu state
    div $ do
      viewSelections state
