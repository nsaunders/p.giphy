module Main where

import Prelude
import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Component.App as App

main :: Effect Unit
main = runHalogenAff (awaitBody >>= runUI App.component unit)
