module Main where

import App (foldp, initialState, view)
import Control.Monad.Eff (Eff)
import Prelude (Unit, bind)
import Pux (CoreEffects, start)
import Pux.Renderer.React (renderToDOM)
import Dom.SelectElement (DOM)

-- | Start and render the app
main :: ∀ fx. Eff (CoreEffects ( dom :: DOM | fx)) Unit
main = do

  app <- start
    { initialState: initialState
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input
