module Data.Image (Image(..), URL, fetchImage) where

import Prelude
import Control.Monad.Except.Trans (ExceptT(..))
import Data.Argonaut (class DecodeJson, Json, JObject, decodeJson, foldJsonObject, getField)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foreign (F, ForeignError(..))
import Data.Maybe (Maybe(Just))
import Data.MediaType.Common (applicationJSON)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (Affjax, get)
import Network.HTTP.Affjax.Response (class Respondable, ResponseType(JSONResponse), fromResponse)

type URL = String

data Image = Image URL

instance responsableImage :: Respondable Image where
  responseType = Tuple (Just applicationJSON) JSONResponse
  fromResponse = fromResponse >=> decodeJsonContent

decodeJsonContent :: forall a. DecodeJson a => Json -> F a
decodeJsonContent json = (ExceptT <<< pure) $ lmap (ForeignError >>> pure) (decodeJson json)

instance decodeImage :: DecodeJson Image where
  decodeJson json = content >>= lookup "data" >>= lookup "image_url" >>= Image >>> pure
    where
      content :: Either String JObject
      content = foldJsonObject (Left "Unexpected response format") Right json
      lookup :: forall a. DecodeJson a => String -> JObject -> Either String a
      lookup = flip getField

type Tag = String

fetchImage :: forall e. Tag -> Affjax e Image
fetchImage tag = get $ "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" <> tag
