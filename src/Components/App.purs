module Component.App (Query, component) where

import Prelude
import Effect.Aff (Aff)
import Data.Image (Image(..), fetchImage)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { tag :: String
  , image :: Maybe Image
  }

data Query a
  = ChangeTag String a
  | FetchImage a

type Input = Unit

type Message = Void

component :: H.Component HH.HTML Query Unit Void Aff
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
          [ HH.input [ HP.placeholder """Enter a tag like "cats" """, HE.onValueInput $ HE.input ChangeTag ]
          , HH.button [ HP.disabled $ state.tag == "", HE.onClick $ HE.input_ FetchImage ] [ HH.text "Fetch" ]
          , HH.div [ HP.class_ $ HC.ClassName "output" ] $ content state.image
        ]
      where
        content Nothing = [ HH.div [ HP.class_ $ HC.ClassName "no-content" ] [ HH.text "Nothing to show yet" ] ]
        content (Just (Image url)) = [ HH.img [HP.src url] ]

    eval :: Query ~> H.ComponentDSL State Query Message Aff
    eval = case _ of
      ChangeTag tag next -> do
        state <- H.get
        let nextState = { tag, image: Nothing }
        H.put nextState
        pure next
      FetchImage next -> do
        state <- H.get
        image <- H.liftAff $ fetchImage state.tag
        let nextState = { tag: state.tag, image: Just image }
        H.put nextState
        pure next
