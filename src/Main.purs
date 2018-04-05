module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Halogen.Aff (HalogenEffects, awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)
import Component.App as App

main :: Eff (HalogenEffects (ajax :: AJAX)) Unit
main = runHalogenAff (awaitBody >>= runUI App.component unit)
