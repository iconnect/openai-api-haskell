{-# OPTIONS_GHC -Wno-unused-imports #-}
-- | The API
module OpenAI.Api where

import Control.Applicative
import Data.Kind
import Data.Proxy
import Data.Sequence ((<|))
import OpenAI.Resources
import Servant.API
import Servant.API.EventStream
import Servant.Auth
import Servant.Auth.Client
import Servant.Client.Core
import Servant.Multipart.API
import Servant.Types.SourceT
import qualified Data.Attoparsec.ByteString       as A
import qualified Data.Attoparsec.ByteString.Char8 as A8
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.ByteString.Char8 as BS8

-- A more robust version of 'NewlineFraming' which aggressively strips all the newlines that
-- occurs between frames, like in the OpenAI case.
data OpenAIFraming

instance FramingUnrender OpenAIFraming where
    framingUnrender _ f = transformWithAtto $ do
        bs <- A.takeWhile (/= 10)
        () <$ (A.takeWhile ((==) 10)) <|> A.endOfInput
        either fail pure (f (LBS.fromStrict bs))

data BearerOrAzureApiKey


-- | @'HasBearer' auths@ is nominally a redundant constraint, but ensures we're not
-- trying to send a token to an API that doesn't accept them.
instance HasClient m api => HasClient m (OpenAIAuthProvider '[BearerOrAzureApiKey] a :> api) where
  type Client m (OpenAIAuthProvider '[BearerOrAzureApiKey] a :> api) = Token -> Client m api

  clientWithRoute m _ req (Token token)
    = clientWithRoute m (Proxy :: Proxy api)
    $ req { requestHeaders = (azureHeader, token)
                          <| ("Authorization", bearerHeaderVal)
                          <| (betaHeader, "assistants=v1")
                          <| requestHeaders req
          }
      where
        bearerHeaderVal = "Bearer " <> token
        azureHeader     = "api-key"
        betaHeader      = "OpenAI-Beta"

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
    :<|> "assistants" :> AssistantsApi
    :<|> "threads" :> ThreadsApi
    :<|> "messages" :> MessagesApi
    :<|> "runs" :> RunsApi

type ModelsApi =
  OpenAIAuth :> Get '[JSON] (OpenAIList Model)
    :<|> OpenAIAuth :> Capture "model_id" ModelId :> Get '[JSON] Model

type CompletionsApi =
  OpenAIAuth :> ReqBody '[JSON] CompletionCreate :> Post '[JSON] CompletionResponse

type ChatApi =
  OpenAIAuth :> "completions"
             :> ReqBody '[JSON] ChatCompletionRequest
             :> QueryParam' '[Optional] "api-version" String
             :> Post '[JSON] ChatResponse

type ChatApiStreaming =
  OpenAIAuth :> "completions"
             :> ReqBody '[JSON] ChatCompletionRequest
             :> QueryParam' '[Optional] "api-version" String
             :> StreamPost NoFraming EventStream EventSource

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

type AssistantsApi =
  OpenAIAuth :> ReqBody '[JSON] AssistantCreate :> Post '[JSON] Assistant
    :<|> OpenAIAuth :> QueryParam "limit" Int
                    :> QueryParam "order" Order
                    :> QueryParam "after" AssistantId
                    :> QueryParam "before" AssistantId
                    :> Get '[JSON] (OpenAIList Assistant)
    :<|> OpenAIAuth :> Capture "assistant_id" AssistantId
                    :> Delete '[JSON] DeleteConfirmation

type ThreadsApi =
  OpenAIAuth :> ReqBody '[JSON] ThreadCreate :> Post '[JSON] Thread

type MessagesApi =
  OpenAIAuth :> "threads" :> Capture "thread_id" ThreadId :> "messages"
             :> QueryParam "limit" Int
             :> QueryParam "order" Order
             :> QueryParam "after" MessageId
             :> QueryParam "before" MessageId
             :> Get '[JSON] (OpenAIList Message)

type RunsApi =
  OpenAIAuth :> Capture "thread_id" ThreadId :> "runs" :> ReqBody '[JSON] RunCreate :> Post '[JSON] Run
    :<|> OpenAIAuth :> "threads" :> "runs"
                    :> ReqBody '[JSON] ThreadAndRunCreate
                    :> Post '[JSON] Run
    :<|> OpenAIAuth :> "threads"
                    :> Capture "thread_id" ThreadId
                    :> "runs"
                    :> Capture "run_id" RunId
                    :> Get '[JSON] Run
