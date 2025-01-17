{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -cpp -pgmP "cpphs --cpp" #-} -- See https://gitlab.haskell.org/ghc/ghc/-/issues/17185#note_327420
{-# OPTIONS_GHC -Wno-orphans             #-}

module OpenAI.Client
  ( -- * Basics
    ApiKey,
    OpenAIClient,
    makeOpenAIClient,
    makeOpenAIClient',
    scBaseUrl,
    ClientError (..),

    -- * Helper types
    TimeStamp (..),
    OpenAIList (..),
    Usage (..),
    DeleteConfirmation(..),

    -- * Models
    Model (..),
    ModelId (..),
    listModels,
    getModel,

    -- * Completion
    CompletionCreate (..),
    CompletionChoice (..),
    CompletionResponse (..),
    defaultCompletionCreate,
    completeText,

    -- * Chat
    ChatFunction (..),
    ChatFunctionCall (..),
    ChatFunctionCallStrategy (..),
    ChatToolChoiceStrategy (..),
    ChatTool (..),
    ChatToolFunction (..),
    ChatToolCall (..),
    ChatMessage (..),
    ChatCompletionRequest (..),
    ChatChoice (..),
    ChatResponse (..),
    Seed(..),
    SystemFingerprint(..),
    ResponseFormat(..),
    ResponseFormatSchema(..),
    ReasoningEffort(..),
    defaultChatCompletionRequest,
    completeChat,
    completeChatStreaming,

    -- * Edits
    EditCreate (..),
    EditChoice (..),
    EditResponse (..),
    createTextEdit,
    defaultEditCreate,

    -- * Images
    ImageResponse (..),
    ImageResponseData (..),
    ImageCreate (..),
    ImageEditRequest (..),
    ImageVariationRequest (..),
    generateImage,
    createImageEdit,
    createImageVariation,

    -- * Embeddings
    EmbeddingCreate (..),
    EmbeddingResponseData (..),
    EmbeddingUsage (..),
    EmbeddingResponse (..),
    createEmbedding,

    -- * Audio
    AudioResponseData (..),
    AudioTranscriptionRequest (..),
    AudioTranslationRequest (..),
    createTranscription,
    createAudioTranslation,

    -- * Engine (deprecated)
    EngineId (..),
    Engine (..),
    listEngines,
    getEngine,

    -- * Engine-based text completion (deprecated)
    TextCompletionId (..),
    TextCompletionChoice (..),
    TextCompletion (..),
    TextCompletionCreate (..),
    defaultEngineTextCompletionCreate,
    engineCompleteText,

    -- * Engine-based embeddings (deprecated)
    EngineEmbeddingCreate (..),
    EngineEmbedding (..),
    engineCreateEmbedding,

    -- * Assistants (BETA)
    Assistant(..),
    AssistantCreate(..),
    AssistantId(..),
    AssistantTool(..),
    Order(..),
    VectorStoreId(..),
    ToolResources(..),
    CodeInterpreterResources(..),
    FileSearchResources(..),
    VectorStoreDescriptor(..),
    createAssistant,
    listAssistants,
    deleteAssistant,

    -- * Threads (BETA)
    Thread(..),
    ThreadCreate(..),
    ThreadId(..),
    createThread,
    deleteThread,

    -- * Messages (BETA)
    MessageId(..),
    Message(..),
    MessageContent(..),
    TextMessage(..),
    MessageAttachment(..),
    getMessages,
    addMessage,

    -- * Runs (BETA)
    Run(..),
    RunCreate(..),
    RunId(..),
    RunErrorCode(..),
    RunError(..),
    RunStatus(..),
    RunRequiredAction(..),
    SubmitToolOutputs(..),
    ToolOutput(..),
    ToolOutputs(..),
    ThreadAndRunCreate(..),
    createRun,
    createThreadAndRun,
    getRun,
    submitToolOutputs,
    cancelRun,

    -- * Fine tunes (out of date)
    FineTuneId (..),
    FineTuneCreate (..),
    defaultFineTuneCreate,
    FineTune (..),
    FineTuneEvent (..),
    createFineTune,
    listFineTunes,
    getFineTune,
    cancelFineTune,
    listFineTuneEvents,

    -- * File API (out of date)
    FileCreate (..),
    File (..),
    FileId (..),
    FileHunk (..),
    FineTuneHunk (..),
    FileDeleteConfirmation (..),
    createFile,
    deleteFile,

    -- * Vector Stores
    VectorStoreCreate(..),
    VectorStore(..),
    VectorStoreStatus(..),
    ChunkingStrategyType(..),
    ChunkingStrategy(..),
    ChunkingStrategyStatic(..),
    ExpiresAfter(..),
    FileSearchOptions(..),
    RankingOptions(..),

    createVectorStore,
    deleteVectorStore
  )
where

import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.ByteString.Lazy as BSL
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Client (Manager)
import OpenAI.Api
import OpenAI.Client.Internal.Helpers
import OpenAI.Resources
import Servant.API
import Servant.API.EventStream
import Servant.Auth.Client
--import Servant.Client
import Servant.Client.Streaming
import qualified Servant.Multipart.Client as MP

-- | Your OpenAI API key. Can be obtained from the OpenAI dashboard. Format: @sk-<redacted>@
type ApiKey = T.Text

-- | Holds a 'Manager' and your API key.
data OpenAIClient = OpenAIClient
  { scBaseUrl :: BaseUrl,
    scToken :: Token,
    scManager :: Manager,
    scMaxRetries :: Int
  }

-- | Construct a 'OpenAIClient'. Note that the passed 'Manager' must support https (e.g. via @http-client-tls@)
makeOpenAIClient' ::
  BaseUrl ->
  ApiKey ->
  Manager ->
  -- | Number of automatic retries the library should attempt.
  Int ->
  OpenAIClient
makeOpenAIClient' u k = OpenAIClient u (Token (T.encodeUtf8 k))

-- | method using default remote base url
makeOpenAIClient ::
  ApiKey ->
  Manager ->
  Int ->
  OpenAIClient
makeOpenAIClient = makeOpenAIClient' openaiBaseUrl

api :: Proxy AzureOpenAIApi
api = Proxy

openaiBaseUrl :: BaseUrl
openaiBaseUrl = BaseUrl Https "api.openai.com" 443 ""

#define EP0(N, R) \
    N##' :: Token -> ClientM R;\
    N :: MonadIO m => OpenAIClient -> m (Either ClientError R);\
    N sc = liftIO . runRequest (scMaxRetries sc) 0 $ runClientM (N##' (scToken sc)) (mkClientEnv (scManager sc) (scBaseUrl sc))

#define EP1(N, ARG, R) \
    N##' :: Token -> ARG -> ClientM R;\
    N :: MonadIO m => OpenAIClient -> ARG -> m (Either ClientError R);\
    N sc a = liftIO . runRequest (scMaxRetries sc) 0 $ runClientM (N##' (scToken sc) a) (mkClientEnv (scManager sc) (scBaseUrl sc))

#define EP2(N, ARG, ARG2, R) \
    N##' :: Token -> ARG -> ARG2 -> ClientM R;\
    N :: MonadIO m => OpenAIClient -> ARG -> ARG2 -> m (Either ClientError R);\
    N sc a b = liftIO . runRequest (scMaxRetries sc) 0 $ runClientM (N##' (scToken sc) a b) (mkClientEnv (scManager sc) (scBaseUrl sc))

#define EP3(N, ARG, ARG2, ARG3, R) \
    N##' :: Token -> ARG -> ARG2 -> ARG3 -> ClientM R;\
    N :: MonadIO m => OpenAIClient -> ARG -> ARG2 -> ARG3 -> m (Either ClientError R);\
    N sc a b c = liftIO . runRequest (scMaxRetries sc) 0 $ runClientM (N##' (scToken sc) a b c) (mkClientEnv (scManager sc) (scBaseUrl sc))

#define EP4(N, ARG, ARG2, ARG3, ARG4, R) \
    N##' :: Token -> ARG -> ARG2 -> ARG3 -> ARG4 -> ClientM R;\
    N :: MonadIO m => OpenAIClient -> ARG -> ARG2 -> ARG3 -> ARG4 -> m (Either ClientError R);\
    N sc a b c d = liftIO . runRequest (scMaxRetries sc) 0 $ runClientM (N##' (scToken sc) a b c d) (mkClientEnv (scManager sc) (scBaseUrl sc))

#define EP5(N, ARG, ARG2, ARG3, ARG4, ARG5, R) \
    N##' :: Token -> ARG -> ARG2 -> ARG3 -> ARG4 -> ARG5 -> ClientM R;\
    N :: MonadIO m => OpenAIClient -> ARG -> ARG2 -> ARG3 -> ARG4 -> ARG5 -> m (Either ClientError R);\
    N sc a b c d e = liftIO . runRequest (scMaxRetries sc) 0 $ runClientM (N##' (scToken sc) a b c d e) (mkClientEnv (scManager sc) (scBaseUrl sc))

#define EP6(N, ARG, ARG2, ARG3, ARG4, ARG5, ARG6, R) \
    N##' :: Token -> ARG -> ARG2 -> ARG3 -> ARG4 -> ARG5 -> ARG6 -> ClientM R;\
    N :: MonadIO m => OpenAIClient -> ARG -> ARG2 -> ARG3 -> ARG4 -> ARG5 -> ARG6 -> m (Either ClientError R);\
    N sc a b c d e f = liftIO . runRequest (scMaxRetries sc) 0 $ runClientM (N##' (scToken sc) a b c d e f) (mkClientEnv (scManager sc) (scBaseUrl sc))

EP0 (listModels, (OpenAIList Model))
EP1 (getModel, ModelId, Model)

EP1 (completeText, CompletionCreate, CompletionResponse)

EP2 (completeChat, ChatCompletionRequest, Maybe String, ChatResponse)

completeChatStreaming :: OpenAIClient
                      -> ChatCompletionRequest
                      -> Maybe String
                      -> (Either ClientError EventSource -> IO a)
                      -> IO a
completeChatStreaming sc rq mb_apiVersion onChunk = do
  let streamingClient = completeChatStreaming' (scToken sc) rq mb_apiVersion
      env             = mkClientEnv (scManager sc) (scBaseUrl sc)
  withClientM streamingClient env onChunk

EP1 (createTextEdit, EditCreate, EditResponse)

EP1 (generateImage, ImageCreate, ImageResponse)
EP1 (createImageEdit, ImageEditRequest, ImageResponse)
EP1 (createImageVariation, ImageVariationRequest, ImageResponse)

EP1 (createEmbedding, EmbeddingCreate, EmbeddingResponse)

createTranscription :: MonadIO m => OpenAIClient -> AudioTranscriptionRequest -> m (Either ClientError AudioResponseData)
createTranscription sc atr =
  do
    bnd <- liftIO MP.genBoundary
    createTranscriptionInternal sc (bnd, atr)

createAudioTranslation :: MonadIO m => OpenAIClient -> AudioTranslationRequest -> m (Either ClientError AudioResponseData)
createAudioTranslation sc atr =
  do
    bnd <- liftIO MP.genBoundary
    createAudioTranslationInternal sc (bnd, atr)

EP1 (createTranscriptionInternal, (BSL.ByteString, AudioTranscriptionRequest), AudioResponseData)
EP1 (createAudioTranslationInternal, (BSL.ByteString, AudioTranslationRequest), AudioResponseData)

createFile :: MonadIO m => OpenAIClient -> FileCreate -> m (Either ClientError File)
createFile sc rfc =
  do
    bnd <- liftIO MP.genBoundary
    createFileInternal sc (bnd, rfc)

EP1 (createFileInternal, (BSL.ByteString, FileCreate), File)
EP1 (deleteFile, FileId, FileDeleteConfirmation)

EP1 (createFineTune, FineTuneCreate, FineTune)
EP0 (listFineTunes, (OpenAIList FineTune))
EP1 (getFineTune, FineTuneId, FineTune)
EP1 (cancelFineTune, FineTuneId, FineTune)
EP1 (listFineTuneEvents, FineTuneId, (OpenAIList FineTuneEvent))

EP0 (listEngines, (OpenAIList Engine))
EP1 (getEngine, EngineId, Engine)
EP2 (engineCompleteText, EngineId, TextCompletionCreate, TextCompletion)
EP2 (engineCreateEmbedding, EngineId, EngineEmbeddingCreate, (OpenAIList EngineEmbedding))

EP2 (createAssistant, Maybe String, AssistantCreate, Assistant)
EP5 (listAssistants, Maybe String, Maybe Int, Maybe Order, Maybe AssistantId, Maybe AssistantId, (OpenAIList Assistant))
EP2 (deleteAssistant, Maybe String, AssistantId, DeleteConfirmation)

EP2 (createThread, Maybe String, ThreadCreate, Thread)
EP2 (deleteThread, Maybe String, ThreadId, DeleteConfirmation)

EP6 (getMessages, Maybe String, ThreadId, Maybe Int, Maybe Order, Maybe MessageId, Maybe MessageId, (OpenAIList Message))
EP3 (addMessage, Maybe String, ThreadId, ThreadMessage, Message)

EP3 (createRun, Maybe String, ThreadId, RunCreate, Run)
EP3 (cancelRun, Maybe String, ThreadId, RunId, Run)
EP2 (createThreadAndRun, Maybe String, ThreadAndRunCreate, Run)
EP3 (getRun, Maybe String, ThreadId, RunId, Run)
EP4 (submitToolOutputs, Maybe String, ThreadId, RunId, ToolOutputs, Run)

EP2 (createVectorStore, Maybe String, VectorStoreCreate, VectorStore)
EP2 (deleteVectorStore, Maybe String, VectorStoreId, DeleteConfirmation)

completeChatStreaming' :: Token -> ChatCompletionRequest -> Maybe String -> ClientM EventSource
( ( listModels'
      :<|> getModel'
    )
    :<|> (completeText')
    :<|> (completeChat' :<|> completeChatStreaming')
    :<|> (createTextEdit')
    :<|> ( generateImage'
             :<|> createImageEdit'
             :<|> createImageVariation'
           )
    :<|> (createEmbedding')
    :<|> ( createTranscriptionInternal'
             :<|> createAudioTranslationInternal'
           )
    :<|> (createFileInternal' :<|> deleteFile')
    :<|> ( createFineTune'
             :<|> listFineTunes'
             :<|> getFineTune'
             :<|> cancelFineTune'
             :<|> listFineTuneEvents'
           )
    :<|> ( listEngines'
             :<|> getEngine'
             :<|> engineCompleteText'
             :<|> engineCreateEmbedding'
           )
    :<|> ( createAssistant'
             :<|> listAssistants'
             :<|> deleteAssistant'
           )
    :<|> ( createThread'
            :<|> createThreadAndRun'
            :<|> getRun'
            :<|> submitToolOutputs'
            :<|> createRun'
            :<|> cancelRun'
            :<|> deleteThread'
            :<|> getMessages'
            :<|> addMessage'
           )
    :<|> ( createVectorStore'
            :<|> deleteVectorStore'
           )
  ) =
    client api
