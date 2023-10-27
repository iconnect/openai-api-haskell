{-# OPTIONS_GHC -Wno-unused-imports #-}
-- | The API
module OpenAI.Api where

import OpenAI.Resources
import Servant.API
import Servant.API.EventStream
import Servant.Auth
import Servant.Auth.Client
import Servant.Multipart.API
import Servant.Client.Core
import Data.Sequence ((<|))
import Data.Proxy
import Data.Kind

data BearerOrAzureApiKey


-- | @'HasBearer' auths@ is nominally a redundant constraint, but ensures we're not
-- trying to send a token to an API that doesn't accept them.
instance HasClient m api => HasClient m (OpenAIAuthProvider '[BearerOrAzureApiKey] a :> api) where
  type Client m (OpenAIAuthProvider '[BearerOrAzureApiKey] a :> api) = Token -> Client m api

  clientWithRoute m _ req (Token token)
    = clientWithRoute m (Proxy :: Proxy api)
    $ req { requestHeaders = (azureHeader, token) <| ("Authorization", bearerHeaderVal) <| requestHeaders req  }
      where
        bearerHeaderVal = "Bearer " <> token
        azureHeader     = "api-key"

  hoistClientMonad pm _ nt cl = hoistClientMonad pm (Proxy :: Proxy api) nt . cl

data OpenAIAuthProvider (auths :: [Type]) val

type OpenAIAuth = OpenAIAuthProvider '[BearerOrAzureApiKey] ()

type OpenAIApi =
  "v1" :> OpenAIApiInternal

-- | The Azure API has no versioning in the URL, that's part of the url query param.
type AzureOpenAIApi =
  OpenAIApiInternal

type OpenAIApiInternal =
  "models" :> ModelsApi
    :<|> "completions" :> CompletionsApi
    :<|> "chat" :> (ChatApi :<|> ChatApiStreaming)
    :<|> "edits" :> EditsApi
    :<|> "images" :> ImagesApi
    :<|> "embeddings" :> EmbeddingsApi
    :<|> "audio" :> AudioApi
    :<|> "files" :> FilesApi
    :<|> FineTuneApi
    :<|> "engines" :> EnginesApi

type ModelsApi =
  OpenAIAuth :> Get '[JSON] (OpenAIList Model)
    :<|> OpenAIAuth :> Capture "model_id" ModelId :> Get '[JSON] Model

type CompletionsApi =
  OpenAIAuth :> ReqBody '[JSON] CompletionCreate :> Post '[JSON] CompletionResponse

type ChatApi =
  OpenAIAuth :> "completions"
             :> ReqBody '[JSON] ChatCompletionRequest
             :> QueryParam' '[Required] "api-version" String
             :> Post '[JSON] ChatResponse

type ChatApiStreaming =
  OpenAIAuth :> "completions"
             :> ReqBody '[JSON] ChatCompletionRequest
             :> QueryParam' '[Required] "api-version" String
             :> StreamPost NewlineFraming EventStream EventSource

type EditsApi =
  OpenAIAuth :> ReqBody '[JSON] EditCreate :> Post '[JSON] EditResponse

type ImagesApi =
  OpenAIAuth :> "generations" :> ReqBody '[JSON] ImageCreate :> Post '[JSON] ImageResponse
    :<|> OpenAIAuth :> "edits" :> ReqBody '[JSON] ImageEditRequest :> Post '[JSON] ImageResponse
    :<|> OpenAIAuth :> "variations" :> ReqBody '[JSON] ImageVariationRequest :> Post '[JSON] ImageResponse

type EmbeddingsApi =
  OpenAIAuth :> ReqBody '[JSON] EmbeddingCreate :> Post '[JSON] EmbeddingResponse

type AudioApi =
  OpenAIAuth :> "transcriptions" :> MultipartForm Tmp AudioTranscriptionRequest :> Post '[JSON] AudioResponseData
    :<|> OpenAIAuth :> "translations" :> MultipartForm Tmp AudioTranslationRequest :> Post '[JSON] AudioResponseData

type FilesApi =
  OpenAIAuth :> MultipartForm Mem FileCreate :> Post '[JSON] File
    :<|> OpenAIAuth :> Capture "file_id" FileId :> Delete '[JSON] FileDeleteConfirmation

type FineTuneApi =
  OpenAIAuth :> "fine-tunes" :> ReqBody '[JSON] FineTuneCreate :> Post '[JSON] FineTune
    :<|> OpenAIAuth :> "fine-tunes" :> Get '[JSON] (OpenAIList FineTune)
    :<|> OpenAIAuth :> "fine-tunes" :> Capture "fine_tune_id" FineTuneId :> Get '[JSON] FineTune
    :<|> OpenAIAuth :> "fine-tunes" :> Capture "fine_tune_id" FineTuneId :> "cancel" :> Post '[JSON] FineTune
    :<|> OpenAIAuth :> "fine-tunes" :> Capture "fine_tune_id" FineTuneId :> "events" :> Get '[JSON] (OpenAIList FineTuneEvent)

type EnginesApi =
  OpenAIAuth :> Get '[JSON] (OpenAIList Engine)
    :<|> OpenAIAuth :> Capture "engine_id" EngineId :> Get '[JSON] Engine
    :<|> OpenAIAuth :> Capture "engine_id" EngineId :> "completions" :> ReqBody '[JSON] TextCompletionCreate :> Post '[JSON] TextCompletion
    :<|> OpenAIAuth :> Capture "engine_id" EngineId :> "embeddings" :> ReqBody '[JSON] EngineEmbeddingCreate :> Post '[JSON] (OpenAIList EngineEmbedding)
