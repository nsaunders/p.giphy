module Component.App (Query, component) where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Image (Image(..), fetchImage)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)

type State =
  { tag :: String
  , image :: Maybe Image
  }

data Query a
  = ChangeTag String a
  | FetchImage a

type Input = Unit

type Message = Void

component :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ajax :: AJAX | eff))
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

    initialState :: State
    initialState = { tag: "", image: Nothing }

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div
        [ HP.class_ $ HC.ClassName "app" ]
        [ HH.div
          [ HP.class_ $ HC.ClassName "input" ]
          [ HH.input [ HP.placeholder """Enter an image tag, like "cats" """, HE.onValueInput $ HE.input ChangeTag ]
          , HH.button [ HP.disabled $ state.tag == "", HE.onClick $ HE.input_ FetchImage ] [ HH.text "Fetch" ]
          ]
        , HH.div [ HP.class_ $ HC.ClassName "output" ] $ content state.image
        ]
      where
        content Nothing = [ HH.div [ HP.class_ $ HC.ClassName "no-content" ] [ HH.text "Nothing to see here" ] ]
        content (Just (Image url)) = [ HH.img [HP.src url] ]

    eval :: Query ~> H.ComponentDSL State Query Message (Aff (ajax :: AJAX | eff))
    eval = case _ of
      ChangeTag tag next -> do
        state <- H.get
        let nextState = { tag, image: Nothing }
        H.put nextState
        pure next
      FetchImage next -> do
        state <- H.get
        imageRequest <- H.liftAff $ fetchImage state.tag
        let nextState = { tag: state.tag, image: Just imageRequest.response }
        H.put nextState
        pure next
