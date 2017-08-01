module Dom.SelectElement
  ( DOM
  , resetDefaultSelected ) where

import Prelude (Unit)
import Control.Monad.Eff (kind Effect, Eff)

-- | DOM Effect
foreign import data DOM :: Effect

-- | We use FFI into javascript just in order to reset the selected option
-- | in the select menu to the default value whenever something else
-- | has been chosen
foreign import resetDefaultSelected :: forall eff. Eff (dom :: DOM | eff) Unit
