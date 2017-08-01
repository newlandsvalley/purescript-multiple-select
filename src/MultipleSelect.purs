module MultipleSelect (Event, State, foldp, initialState, view) where


import Prelude (bind, const, discard, pure, ($), (/=))
import Pux (EffModel, noEffects)
import Pux.DOM.Events (onClick, onChange, targetValue)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (a, div, label, option, select, span, ul, li)
import Text.Smolder.HTML.Attributes as At
import Text.Smolder.Markup (text, (!), (#!), (!?))
import Data.Array (elem, filter, reverse, (:))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Control.Monad.Eff.Class (liftEff)

import Pux.DOM.HTML.Attributes (style)
import Dom.SelectElement

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
  , label :: String                -- the label (if needed for) the drop down
  , available :: Array String      -- available options
  , selected  :: Array String      -- currently selected options
  }

initialState :: String -> String -> Array String -> State
initialState instruction label available = {
    instruction : instruction
  , label : label
  , available : available
  , selected : []
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
      label ! At.for "selection-menu" $ do
        text state.label
      select  ! At.id "selection-menu"
        #! onChange (\e -> AddSelection (targetValue e) )
          $ do
            option ! At.selected "selected" ! At.disabled "disabled" $ text state.instruction
            traverse_ f state.available

-- | add a selection to the end of the list
addSelection :: String -> Array String -> Array String
addSelection s ss =
  reverse $ s : (reverse ss)

-- | remove a selection from the list
removeSelection :: String -> Array String -> Array String
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
