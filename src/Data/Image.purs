module Data.Image (Image(..), fetchImage) where

import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core (caseJsonObject)
import Data.Argonaut.Decode (class DecodeJson, (.?), decodeJson)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Exception (error)
import Network.HTTP.Affjax (URL, get)
import Network.HTTP.Affjax.Response (json)

data Image = Image URL

instance decodeImage :: DecodeJson Image where
  decodeJson json = do
    content <- caseJsonObject (Left "Unexpected response format") Right json
    dataContent <- content .? "data"
    url <- dataContent .? "image_url"
    pure $ Image url

type Tag = String

fetchImage :: Tag -> Aff Image
fetchImage tag = do
  req <- get json $ "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" <> tag
  case (decodeJson req.response) of
    Left err ->
      throwError (error "Unexpected response format")
    Right image ->
      pure image
