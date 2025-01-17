{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module OpenAI.Resources
  ( -- * Core Types
    TimeStamp (..),
    OpenAIList (..),
    Usage (..),
    DeleteConfirmation(..),

    -- * Models
    Model (..),
    ModelId (..),

    -- * Completion
    CompletionCreate (..),
    CompletionChoice (..),
    CompletionResponse (..),
    defaultCompletionCreate,

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

    -- * Chat streaming
    ChatChoiceChunk (..),
    ChatChoiceDelta (..),
    ChatResponseChunk (..),

    -- * Edits
    EditCreate (..),
    EditChoice (..),
    EditResponse (..),
    defaultEditCreate,

    -- * Images
    ImageResponse (..),
    ImageResponseData (..),
    ImageCreate (..),
    ImageEditRequest (..),
    ImageVariationRequest (..),

    -- * Embeddings
    EmbeddingCreate (..),
    EmbeddingResponseData (..),
    EmbeddingUsage (..),
    EmbeddingResponse (..),

    -- * Audio
    AudioResponseData (..),
    AudioTranscriptionRequest (..),
    AudioTranslationRequest (..),

    -- * Fine tuning (out of date)
    FineTuneId (..),
    FineTuneCreate (..),
    defaultFineTuneCreate,
    FineTune (..),
    FineTuneEvent (..),

    -- * File API (out of date)
    FileCreate (..),
    FileId (..),
    File (..),
    FileHunk (..),
    FineTuneHunk (..),
    FileDeleteConfirmation (..),

    -- * Engine (deprecated)
    EngineId (..),
    Engine (..),

    -- * Engine text completion (deprecated)
    TextCompletionId (..),
    TextCompletionChoice (..),
    TextCompletion (..),
    TextCompletionCreate (..),
    defaultEngineTextCompletionCreate,

    -- * Engine Embeddings (deprecated)
    EngineEmbeddingCreate (..),
    EngineEmbedding (..),

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

    -- * Threads (BETA)
    Thread(..),
    ThreadCreate(..),
    ThreadId(..),
    ThreadMessage(..),

    -- * Messages (BETA)
    MessageId(..),
    Message(..),
    MessageContent(..),
    TextMessage(..),
    MessageAttachment(..),

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

    -- * Vector Stores
    VectorStoreCreate(..),
    VectorStore(..),
    VectorStoreStatus(..),
    ChunkingStrategyType(..),
    ChunkingStrategy(..),
    ChunkingStrategyStatic(..),
    ExpiresAfter(..),
    FileSearchOptions(..),
    RankingOptions(..)
  )
where

import Control.DeepSeq
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Vector as V
import GHC.Generics
import Network.Mime (defaultMimeLookup)
import OpenAI.Internal.Aeson
import Servant.API
import Servant.Multipart.API

-- | A 'UTCTime' wrapper that has unix timestamp JSON representation
newtype TimeStamp = TimeStamp {unTimeStamp :: UTCTime}
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

instance A.ToJSON TimeStamp where
  toJSON = A.Number . fromRational . toRational . utcTimeToPOSIXSeconds . unTimeStamp

instance A.FromJSON TimeStamp where
  parseJSON =
    A.withScientific "unix timestamp" $ \sci ->
      pure $ TimeStamp $ posixSecondsToUTCTime (fromRational $ toRational sci)

instance ToHttpApiData TimeStamp where
  toUrlPiece x =
    let unix :: Int
        unix = round . utcTimeToPOSIXSeconds . unTimeStamp $ x
     in T.pack (show unix)

-- | A 'V.Vector' wrapper.
newtype OpenAIList a = OpenAIList
  { olData :: V.Vector a
  }
  deriving stock (Show, Eq, Functor, Generic)
  deriving anyclass NFData

instance Semigroup (OpenAIList a) where
  (<>) a b = OpenAIList (olData a <> olData b)

instance Monoid (OpenAIList a) where
  mempty = OpenAIList mempty

instance Applicative OpenAIList where
  pure = OpenAIList . pure
  (<*>) go x = OpenAIList (olData go <*> olData x)

$(deriveJSON (jsonOpts 2) ''OpenAIList)

data Usage = Usage
  { usPromptTokens :: Int,
    usCompletionTokens :: Int,
    usTotalTokens :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 2) ''Usage)

-- | Generic delete confirmation
data DeleteConfirmation = DeleteConfirmation
  { dcId :: T.Text
  , dcObject :: T.Text
  , dcDeleted :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 2) ''DeleteConfirmation)

------------------------
------ Model API
------------------------

data Model = Model
  { mId :: ModelId,
    mObject :: T.Text,
    mOwnedBy :: T.Text,
    mPermission :: [A.Object] -- TODO 2023.03.22: Docs do not say what this is
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

newtype ModelId = ModelId {unModelId :: T.Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 1) ''Model)

------------------------
------ Completions API
------------------------

data CompletionCreate = CompletionCreate
  { ccrModel :: ModelId,
    ccrPrompt :: Maybe T.Text,
    ccrSuffix :: Maybe T.Text,
    ccrMaxTokens :: Maybe Int,
    ccrTemperature :: Maybe Double,
    ccrTopP :: Maybe Double,
    ccrN :: Maybe Int,
    ccrStream :: Maybe Bool,
    ccrLogprobs :: Maybe Int,
    ccrEcho :: Maybe Bool,
    ccrStop :: Maybe (V.Vector T.Text),
    ccrPresencePenalty :: Maybe Double,
    ccrFrequencyPenalty :: Maybe Double,
    ccrBestOf :: Maybe Int,
    ccrLogitBias :: Maybe (V.Vector Double),
    ccrUser :: Maybe String
  }
  deriving (Show, Eq)

defaultCompletionCreate :: ModelId -> T.Text -> CompletionCreate
defaultCompletionCreate model prompt =
  CompletionCreate
    { ccrModel = model,
      ccrPrompt = Just prompt,
      ccrSuffix = Nothing,
      ccrMaxTokens = Nothing,
      ccrTemperature = Nothing,
      ccrTopP = Nothing,
      ccrN = Nothing,
      ccrStream = Nothing,
      ccrLogprobs = Nothing,
      ccrEcho = Nothing,
      ccrStop = Nothing,
      ccrPresencePenalty = Nothing,
      ccrFrequencyPenalty = Nothing,
      ccrBestOf = Nothing,
      ccrLogitBias = Nothing,
      ccrUser = Nothing
    }

data CompletionChoice = CompletionChoice
  { cchText :: T.Text,
    cchIndex :: Int,
    cchLogprobs :: Maybe (V.Vector Double),
    cchFinishReason :: Maybe T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

data CompletionResponse = CompletionResponse
  { crId :: T.Text,
    crObject :: T.Text,
    crCreated :: Int,
    crModel :: ModelId,
    crChoices :: [CompletionChoice],
    crUsage :: A.Object
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 3) ''CompletionCreate)
$(deriveJSON (jsonOpts 3) ''CompletionChoice)
$(deriveJSON (jsonOpts 2) ''CompletionResponse)

------------------------
------ Chat API
------------------------

data ChatFunctionCall = ChatFunctionCall
  { chfcName :: Maybe T.Text,
    chfcArguments :: T.Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NFData

instance A.FromJSON ChatFunctionCall where
  parseJSON = A.withObject "ChatFunctionCall" $ \obj -> do
    name      <- obj A..:? "name"
    arguments <- obj A..: "arguments"

    pure $ ChatFunctionCall {chfcName = name, chfcArguments = arguments}

instance A.ToJSON ChatFunctionCall where
  toJSON (ChatFunctionCall {chfcName = name, chfcArguments = arguments}) =
    A.object
      [ "name" A..= name,
        "arguments" A..= arguments
      ]

data ChatToolCall = ChatToolCall
  { -- | The ID of the tool call.
    ctcId :: T.Text,
    -- | The type of the tool. Currently, only function is supported.
    ctcType :: T.Text,
    -- The function that the model called.
    ctcFunction :: ChatFunctionCall
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 3) ''ChatToolCall)

data ChatMessage = ChatMessage
  { chmContent :: Maybe T.Text,
    chmRole :: T.Text,
    -- /Deprecated/ and replaced by tool_calls.
    -- The name and arguments of a function that should be called, as generated by the model.
    chmFunctionCall :: Maybe ChatFunctionCall,
    -- The tool calls generated by the model, such as function calls.
    chmToolCalls :: Maybe [ChatToolCall],
    chmName :: Maybe T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

instance A.FromJSON ChatMessage where
  parseJSON = A.withObject "ChatMessage" $ \obj ->
    ChatMessage <$> obj A..:? "content"
                <*> obj A..: "role"
                <*> obj A..:? "function_call"
                <*> obj A..:?  "tool_calls"
                <*> obj A..:? "name"

instance A.ToJSON ChatMessage where
  toJSON (ChatMessage {chmContent = content, chmRole = role, chmFunctionCall = functionCall, chmName = name, chmToolCalls = toolCalls}) =
    A.object $ 
      [ "content" A..= content,
        "role" A..= role
      ] ++ catMaybes
      [ ("function_call" A..=) <$> functionCall,
        ("tool_calls" A..=) <$> toolCalls,
        ("name" A..=) <$> name
      ]
      
data ChatFunction = ChatFunction
  { chfName :: T.Text,
    chfDescription :: T.Text,
    chfParameters :: A.Value
  }
  deriving (Show, Eq)

data ChatFunctionCallStrategy =
    CFCS_auto
  | CFCS_none
  | CFCS_name T.Text
  deriving (Show, Eq)

instance ToJSON ChatFunctionCallStrategy where
  toJSON = \case
    CFCS_auto              -> A.String "auto"
    CFCS_none              -> A.String "none"
    CFCS_name functionName -> A.object [ "name" A..= A.toJSON functionName ]

instance FromJSON ChatFunctionCallStrategy where
  parseJSON (A.String "auto") = pure CFCS_auto
  parseJSON (A.String "none") = pure CFCS_none
  parseJSON xs = flip (A.withObject "ChatFunctionCallStrategy") xs $ \o -> do
    functionName <- o A..: "name"
    pure $ CFCS_name functionName

data ChatToolChoiceStrategy =
    CFTS_auto
  | CFTS_none
  | CFTS_function T.Text
  deriving (Show, Eq, Generic)
  deriving anyclass NFData

instance ToJSON ChatToolChoiceStrategy where
  toJSON = \case
    CFTS_auto                  -> A.String "auto"
    CFTS_none                  -> A.String "none"
    CFTS_function functionName -> A.object [ "type" A..= A.String "function"
                                           , "function" A..= A.object [ "name" A..= A.toJSON functionName ]
                                           ]

instance FromJSON ChatToolChoiceStrategy where
  parseJSON (A.String "auto") = pure CFTS_auto
  parseJSON (A.String "none") = pure CFTS_none
  parseJSON xs = flip (A.withObject "ChatToolChoiceStrategy") xs $ \o -> do
    toolType     <- o A..: "type"
    case toolType of
      "function" -> do
        funcBlob     <- o        A..: "function"
        functionName <- funcBlob A..: "name"
        pure $ CFTS_function functionName
      unsupportedType
        -> A.typeMismatch ("Unsupported tool type: " <> T.unpack unsupportedType) (A.String unsupportedType)

data ChatTool = ChatTool
  { chtType :: T.Text,
    chfFunction :: ChatToolFunction
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

data ChatToolFunction = ChatToolFunction
  { chtfDescription :: Maybe T.Text,
    chtfName   :: T.Text,
    chtfStrict :: Maybe Bool,
    chtfParameters :: Maybe A.Value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

-- This feature is in Beta. If specified, our system will make a best effort to
-- sample deterministically, such that repeated requests with the same seed and
-- parameters should return the same result. Determinism is not guaranteed, and
-- you should refer to the system_fingerprint response parameter to monitor
-- changes in the backend.
newtype Seed = Seed { unSeed :: Int }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData)
  deriving anyclass NFData

data ReasoningEffort =
    RE_low
  | RE_medium
  | RE_high
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 3) ''ReasoningEffort)

data ChatCompletionRequest = ChatCompletionRequest
  { chcrModel :: ModelId,
    chcrMessages :: [ChatMessage],
    chcrStore :: Maybe Bool,
    chcrReasoningEffort :: Maybe ReasoningEffort,
    -- | A list of functions the model may generate JSON inputs for.
    -- /Deprecated/ by OpenAI in favour of \"tools\".
    chcrFunctions :: Maybe [ChatFunction],
    -- | The function to call. /deprecated/ by OpenAI in favour of \"tool_choice\".
    chcrFunctionCall :: Maybe ChatFunctionCallStrategy,
    chcrToolChoice :: Maybe ChatToolChoiceStrategy,
    chcrTools      :: Maybe [ChatTool],
    chcrTemperature :: Maybe Double,
    chcrTopP :: Maybe Double,
    chcrN :: Maybe Int,
    chcrSeed :: Maybe Seed,
    chcrStream :: Maybe Bool,
    chcrStop :: Maybe (V.Vector T.Text),
    -- | DEPRECATED: Please use chcrMaxCompletionTokens
    chcrMaxTokens :: Maybe Int,
    chcrMaxCompletionTokens :: Maybe Int,
    chcrPresencePenalty :: Maybe Double,
    chcrResponseFormat :: Maybe ResponseFormat,
    chcrFrequencyPenalty :: Maybe Double,
    chcrLogitBias :: Maybe (V.Vector Double),
    chcrParallelToolCalls :: Maybe Bool,
    chcrUser :: Maybe String
  }
  deriving (Show, Eq)

data ResponseFormat
  = RF_text
  | RF_json_object
  | RF_json_schema ResponseFormatSchema
  deriving (Show, Eq, Generic)
  deriving anyclass NFData

data ResponseFormatSchema = ResponseFormatSchema
  { rfsName   :: T.Text
  , rfsStrict :: Bool
  , rfsSchema :: A.Value
  } deriving (Show, Eq, Generic)
    deriving anyclass NFData

$(deriveJSON (jsonOpts 3) ''ResponseFormatSchema)

instance ToJSON ResponseFormat where
  toJSON = \case
    RF_text
      -> A.object [ "type" A..= A.String "text" ]
    RF_json_object
      -> A.object [ "type" A..= A.String "json_object" ]
    RF_json_schema schema
      -> A.object [ "type" A..= A.String "json_schema"
                  , "json_schema" A..= schema
                  ]

instance FromJSON ResponseFormat where
  parseJSON = A.withObject "ResponseFormat" $ \o -> do
    rt <- o A..: "type"
    case rt of
      "text"
        -> pure RF_text
      "json_object"
        -> pure RF_json_object
      "json_schema"
        -> do
          s <- o A..: "json_schema"
          pure $ RF_json_schema s
      xs
        -> fail $ "ResponseFormat unexpected type: " <> T.unpack xs

defaultChatCompletionRequest :: ModelId -> [ChatMessage] -> ChatCompletionRequest
defaultChatCompletionRequest model messages =
  ChatCompletionRequest
    { chcrModel = model,
      chcrMessages = messages,
      chcrStore = Nothing,
      chcrReasoningEffort = Nothing,
      chcrFunctions = Nothing,
      chcrFunctionCall = Nothing,
      chcrToolChoice = Nothing,
      chcrTools = Nothing,
      chcrTemperature = Nothing,
      chcrTopP = Nothing,
      chcrN = Nothing,
      chcrSeed = Nothing,
      chcrStream = Nothing,
      chcrStop = Nothing,
      chcrMaxTokens = Nothing,
      chcrMaxCompletionTokens = Nothing,
      chcrPresencePenalty = Nothing,
      chcrResponseFormat = Nothing,
      chcrFrequencyPenalty = Nothing,
      chcrLogitBias = Nothing,
      chcrUser = Nothing,
      chcrParallelToolCalls = Nothing
    }

data ChatChoice = ChatChoice
  { chchIndex :: Int,
    chchMessage :: ChatMessage,
    chchFinishReason :: Maybe T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

data ChatChoiceChunk = ChatChoiceChunk
  { chccIndex        :: Int,
    chccFinishReason :: Maybe T.Text,
    chccDelta        :: ChatChoiceDelta
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

data ChatChoiceDelta =
    NoChatChoiceDelta
  | ChatChoiceDelta (Maybe T.Text) (Maybe ChatFunctionCall)
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

instance ToJSON ChatChoiceDelta where
  toJSON = \case
    NoChatChoiceDelta          -> A.object []
    ChatChoiceDelta content fc -> A.object [ "content" A..= A.toJSON content, "function_call" A..= A.toJSON fc ]

instance FromJSON ChatChoiceDelta where
  parseJSON = A.withObject "ChatChoiceDelta" $ \o -> do
    case KM.null o of
      True  -> pure $ NoChatChoiceDelta
      False -> do
        functionCall <- o A..:? "function_call"
        content      <- o A..:? "content"
        pure $ ChatChoiceDelta content functionCall

-- | Represents the backend configuration that the model runs with.
-- Can be used in conjunction with the seed request parameter to understand
-- when backend changes have been made that might impact determinism.
newtype SystemFingerprint = SystemFingerprint { unSystemFingerprint :: T.Text }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData)
  deriving anyclass NFData


data ChatResponse = ChatResponse
  { chrId :: T.Text,
    chrObject :: T.Text,
    chrCreated :: Int,
    chrChoices :: [ChatChoice],
    chrSystemFingerprint :: SystemFingerprint,
    chrUsage :: Usage
  } deriving stock Generic
    deriving anyclass NFData

$(deriveJSON (jsonOpts 3) ''ChatFunction)
$(deriveJSON (jsonOpts 4) ''ChatToolFunction)
$(deriveJSON (jsonOpts 3) ''ChatTool)
$(deriveJSON (jsonOpts 4) ''ChatCompletionRequest)
$(deriveJSON (jsonOpts 4) ''ChatChoice)
$(deriveJSON (jsonOpts 4) ''ChatChoiceChunk)
$(deriveJSON (jsonOpts 3) ''ChatResponse)

-- | Chat completion object we get from a streaming chat completion.
data ChatResponseChunk = ChatResponseChunk
  { chrcId      :: T.Text,
    chrcObject  :: T.Text,
    chrcCreated :: Int,
    chrcChoices :: [ChatChoiceChunk]
  } deriving stock (Generic, Show, Eq)
    deriving anyclass NFData

$(deriveJSON (jsonOpts 4) ''ChatResponseChunk)

------------------------
------ Edits API
------------------------

data EditCreate = EditCreate
  { edcrModel :: ModelId,
    edcrInput :: Maybe T.Text,
    edcrInstruction :: T.Text,
    edcrN :: Maybe Int,
    edcrTemperature :: Maybe Double,
    edcrTopP :: Maybe Double
  }
  deriving (Show, Eq)

defaultEditCreate :: ModelId -> T.Text -> T.Text -> EditCreate
defaultEditCreate model input instruction =
  EditCreate
    { edcrModel = model,
      edcrInput = Just input,
      edcrInstruction = instruction,
      edcrN = Nothing,
      edcrTemperature = Nothing,
      edcrTopP = Nothing
    }

data EditChoice = EditChoice
  { edchText :: T.Text,
    edchIndex :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

data EditResponse = EditResponse
  { edrObject :: T.Text,
    edrCreated :: Int,
    edrChoices :: [EditChoice],
    edrUsage :: Usage
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 4) ''EditCreate)
$(deriveJSON (jsonOpts 4) ''EditChoice)
$(deriveJSON (jsonOpts 3) ''EditResponse)

------------------------
------ Images API
------------------------

data ImageResponseData = ImageResponseData
  { irdUrl :: T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

data ImageResponse = ImageResponse
  { irCreated :: TimeStamp,
    irData :: [ImageResponseData]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 3) ''ImageResponseData)
$(deriveJSON (jsonOpts 2) ''ImageResponse)

-- | Image create API
data ImageCreate = ImageCreate
  { icPrompt :: T.Text,
    icN :: Maybe Int,
    icSize :: Maybe T.Text,
    icResponseFormat :: Maybe T.Text,
    icUser :: Maybe T.Text
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 2) ''ImageCreate)

-- | Image edit API
data ImageEditRequest = ImageEditRequest
  { ierImage :: T.Text,
    ierMask :: Maybe T.Text,
    ierPrompt :: T.Text,
    ierN :: Maybe Int,
    ierSize :: Maybe T.Text,
    ierResponseFormat :: Maybe T.Text,
    ierUser :: Maybe T.Text
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 3) ''ImageEditRequest)

-- | Image variation API
data ImageVariationRequest = ImageVariationRequest
  { ivrImage :: T.Text,
    ivrN :: Maybe Int,
    ivrSize :: Maybe T.Text,
    ivrResponseFormat :: Maybe T.Text,
    ivrUser :: Maybe T.Text
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 3) ''ImageVariationRequest)

------------------------
------ Embeddings API
------------------------

data EmbeddingCreate = EmbeddingCreate
  { embcModel :: ModelId,
    embcInput :: T.Text, -- TODO (2023.02.23): Extend to allow taking in array of strings or token arrays
    embcUser :: Maybe T.Text
  }
  deriving (Show, Eq)

data EmbeddingResponseData = EmbeddingResponseData
  { embdObject :: T.Text,
    embdEmbedding :: V.Vector Double,
    embdIndex :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

data EmbeddingUsage = EmbeddingUsage
  { embuPromptTokens :: Int,
    embuTotalTokens :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

data EmbeddingResponse = EmbeddingResponse
  { embrObject :: T.Text,
    embrData :: [EmbeddingResponseData],
    embrModel :: ModelId,
    embrUsage :: EmbeddingUsage
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 4) ''EmbeddingCreate)
$(deriveJSON (jsonOpts 4) ''EmbeddingResponseData)
$(deriveJSON (jsonOpts 4) ''EmbeddingUsage)
$(deriveJSON (jsonOpts 4) ''EmbeddingResponse)

------------------------
------ Audio API
------------------------

data AudioResponseData = AudioResponseData
  { audrdText :: T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 5) ''AudioResponseData)

-- | Audio create API
data AudioTranscriptionRequest = AudioTranscriptionRequest
  { audtsrFile :: FilePath,
    audtsrModel :: ModelId,
    audtsrPrompt :: Maybe T.Text,
    audtsrResponseFormat :: Maybe T.Text,
    audtsrTemperature :: Maybe Double,
    audtsrLanguage :: Maybe T.Text
  }
  deriving (Show, Eq)

instance ToMultipart Tmp AudioTranscriptionRequest where
  toMultipart atr =
    MultipartData
      ( catMaybes
          [ Input "model" . unModelId <$> Just (audtsrModel atr),
            Input "prompt" <$> audtsrPrompt atr,
            Input "response_format" <$> audtsrResponseFormat atr,
            Input "temperature" . T.pack . show <$> audtsrTemperature atr,
            Input "language" <$> audtsrLanguage atr
          ]
      )
      [ FileData "file" (T.pack . audtsrFile $ atr) (T.decodeUtf8 . defaultMimeLookup . T.pack $ audtsrFile atr) (audtsrFile atr)
      ]

$(deriveJSON (jsonOpts 6) ''AudioTranscriptionRequest)

-- | Audio translation API
data AudioTranslationRequest = AudioTranslationRequest
  { audtlrFile :: FilePath,
    audtlrModel :: ModelId,
    audtlrPrompt :: Maybe T.Text,
    audtlrResponseFormat :: Maybe T.Text,
    audtlrTemperature :: Maybe Double
  }
  deriving (Show, Eq)

instance ToMultipart Tmp AudioTranslationRequest where
  toMultipart atr =
    MultipartData
      ( catMaybes
          [ Input "model" . unModelId <$> Just (audtlrModel atr),
            Input "prompt" <$> audtlrPrompt atr,
            Input "response_format" <$> audtlrResponseFormat atr,
            Input "temperature" . T.pack . show <$> audtlrTemperature atr
          ]
      )
      [ FileData "file" (T.pack . audtlrFile $ atr) (T.decodeUtf8 . defaultMimeLookup . T.pack $ audtlrFile atr) (audtlrFile atr)
      ]

$(deriveJSON (jsonOpts 6) ''AudioTranslationRequest)

------------------------
------ Files API
------------------------

data FineTuneHunk = FineTuneHunk
  { fthPrompt :: T.Text,
    fthCompletion :: T.Text
  }
  deriving (Show, Eq)

data FileHunk
  = FhFineTune FineTuneHunk
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 3) ''FineTuneHunk)

newtype FileId = FileId {unFileId :: T.Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData)
  deriving anyclass NFData

data File = File
  { fId :: FileId,
    fObject :: T.Text,
    fBytes :: Int,
    fCreatedAt :: TimeStamp,
    fFilename :: T.Text,
    fPurpose :: T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 1) ''File)

-- | File upload API
data FileCreate = FileCreate
  { fcPurpose :: T.Text,
    fcDocuments :: [FileHunk]
  }
  deriving (Show, Eq)

packDocuments :: [FileHunk] -> BSL.ByteString
packDocuments docs =
  BSL.intercalate "\n" $
    map
      ( \t -> A.encode $
          case t of
            FhFineTune x -> A.toJSON x
      )
      docs

instance ToMultipart Mem FileCreate where
  toMultipart rfc =
    MultipartData
      [ Input "purpose" (fcPurpose rfc)
      ]
      [ FileData "file" "data.jsonl" "application/json" (packDocuments $ fcDocuments rfc)
      ]

-- | File delete API
data FileDeleteConfirmation = FileDeleteConfirmation
  { fdcId :: FileId
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 3) ''FileDeleteConfirmation)

-- | File retrieve API
-- TODO

-- | File retrieve content API
-- TODO

------------------------
------ Engine API (deprecated)
------------------------

newtype EngineId = EngineId {unEngineId :: T.Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData)
  deriving anyclass NFData

data Engine = Engine
  { eId :: EngineId,
    eOwner :: T.Text,
    eReady :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 1) ''Engine)

------------------------
------ Engine completions API (deprecated)
------------------------

newtype TextCompletionId = TextCompletionId {unTextCompletionId :: T.Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData)
  deriving anyclass NFData

data TextCompletionChoice = TextCompletionChoice
  { tccText :: T.Text,
    tccIndex :: Int,
    tccLogProps :: Maybe Int,
    tccFinishReason :: T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

data TextCompletion = TextCompletion
  { tcId :: TextCompletionId,
    tcCreated :: TimeStamp,
    tcModel :: T.Text,
    tcChoices :: V.Vector TextCompletionChoice
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

data TextCompletionCreate = TextCompletionCreate
  { tccrPrompt :: T.Text, -- TODO: support lists of strings
    tccrMaxTokens :: Maybe Int,
    tccrTemperature :: Maybe Double,
    tccrTopP :: Maybe Double,
    tccrN :: Maybe Int,
    tccrLogprobs :: Maybe Int,
    tccrEcho :: Maybe Bool,
    tccrStop :: Maybe (V.Vector T.Text),
    tccrPresencePenalty :: Maybe Double,
    tccrFrequencyPenalty :: Maybe Double,
    tccrBestOf :: Maybe Int
  }
  deriving (Show, Eq)

-- | Applies API defaults, only passing a prompt.
defaultEngineTextCompletionCreate :: T.Text -> TextCompletionCreate
defaultEngineTextCompletionCreate prompt =
  TextCompletionCreate
    { tccrPrompt = prompt,
      tccrMaxTokens = Nothing,
      tccrTemperature = Nothing,
      tccrTopP = Nothing,
      tccrN = Nothing,
      tccrLogprobs = Nothing,
      tccrEcho = Nothing,
      tccrStop = Nothing,
      tccrPresencePenalty = Nothing,
      tccrFrequencyPenalty = Nothing,
      tccrBestOf = Nothing
    }

$(deriveJSON (jsonOpts 3) ''TextCompletionChoice)
$(deriveJSON (jsonOpts 2) ''TextCompletion)
$(deriveJSON (jsonOpts 4) ''TextCompletionCreate)

------------------------
------ EngineEmbeddings API (deprecated)
------------------------

data EngineEmbeddingCreate = EngineEmbeddingCreate
  {enecInput :: T.Text}
  deriving (Show, Eq)

data EngineEmbedding = EngineEmbedding
  {eneEmbedding :: V.Vector Double, eneIndex :: Int}
  deriving stock (Show, Eq, Generic)
  deriving NFData

$(deriveJSON (jsonOpts 4) ''EngineEmbeddingCreate)
$(deriveJSON (jsonOpts 3) ''EngineEmbedding)

newtype AssistantId = AssistantId {unAssistantId :: T.Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData)
  deriving anyclass NFData

data RankingOptions = RankingOptions
  { ropRanker :: Maybe String
  , ropScoreThreshold :: Maybe Double
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

data FileSearchOptions = FileSearchOptions
  { fsoMaxNumResults :: Maybe Int
  , fsoRankingOptions :: Maybe RankingOptions
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 3) ''RankingOptions)
$(deriveJSON (jsonOpts 3) ''FileSearchOptions)

data AssistantTool =
    AT_code_interpreter
  -- | DEPRECATED in version v2 of the assistant API
  | AT_retrieval
  | AT_file_search (Maybe FileSearchOptions)
  | AT_function ChatTool
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

instance ToJSON AssistantTool where
  toJSON = \case
    AT_code_interpreter -> A.object [ "type" A..= A.String "code_interpreter" ]
    AT_retrieval        -> A.object [ "type" A..= A.String "retrieval" ]
    AT_file_search Nothing -> A.object [ "type" A..= A.String "file_search" ]
    AT_file_search (Just opts) -> A.object [ "type" A..= A.String "file_search"
                                           , "file_search" A..= A.toJSON opts
                                           ]
    AT_function ct      -> A.toJSON ct

instance FromJSON AssistantTool where
  parseJSON = A.withObject "AssistantTool" $ \o -> do
    ty <- o A..: "type"
    case ty of
      "code_interpreter" -> pure AT_code_interpreter
      "retrieval"        -> pure AT_retrieval
      "file_search"      -> AT_file_search <$> (o A..:? "file_search")
      "function"         -> AT_function <$> A.parseJSON (A.Object o)
      _                  -> A.typeMismatch ("AssistantTool, invalid type: " <> show ty) ty

data CodeInterpreterResources = CodeInterpreterResources
  { cirFileIds :: Maybe [FileId]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 3) ''CodeInterpreterResources)

newtype VectorStoreId = VectorStoreId {unVectorStoreId :: T.Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData)
  deriving anyclass NFData

data VectorStoreDescriptor = VectorStoreDescriptor
  { vsdFileIds  :: Maybe [FileId]
  , vsdMetadata :: Maybe A.Value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 3) ''VectorStoreDescriptor)

data FileSearchResources = FileSearchResources
  { fsrVectorStoreIds :: Maybe [VectorStoreId]
  , fsrVectorStores   :: Maybe [VectorStoreDescriptor]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 3) ''FileSearchResources)

data ToolResources = ToolResources
  { trsCodeInterpreter :: Maybe CodeInterpreterResources
  , trsFileSearch      :: Maybe FileSearchResources
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 3) ''ToolResources)

data Assistant = Assistant
  { astId :: AssistantId
  , astObject :: T.Text
  , astCreatedAt :: TimeStamp
  , astName :: Maybe T.Text
  , astDescription :: Maybe T.Text
  , astModel :: ModelId
  , astInstructions :: Maybe T.Text
  , astTools :: [AssistantTool]
  , astToolResources :: Maybe ToolResources
  -- | DEPRECATED removed in version v2
  , astFileIds :: Maybe [FileId]
  , astMetadata :: A.Value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 3) ''Assistant)

data AssistantCreate = AssistantCreate
  { acrModel :: ModelId
  , acrName  :: Maybe T.Text
  , acrDescription :: Maybe T.Text
  , acrInstructions :: Maybe T.Text
  , acrTools :: Maybe [AssistantTool]
  -- | DEPRECATED: Remove in version v2 of the assistant API
  , acrFileIds :: Maybe [FileId]
  , acrToolResources :: Maybe ToolResources
  , acrMetadata :: Maybe A.Value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 3) ''AssistantCreate)

newtype ThreadId = ThreadId {unThreadId :: T.Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData)
  deriving anyclass NFData

data Thread = Thread
  { thrId :: ThreadId
  , thrObject :: T.Text
  , thrCreatedAt :: TimeStamp
  , thrToolResources :: Maybe ToolResources
  , thrTools    :: Maybe AssistantTool
  , thrMetadata :: A.Value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 3) ''Thread)

data MessageAttachment = MessageAttachment
  { matFileId :: Maybe FileId
  , matTools  :: Maybe [AssistantTool]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 3) ''MessageAttachment)

data ThreadMessage = ThreadMessage
  { -- | Currently only \"user\" is supported.
    thrmRole    :: T.Text
  , thrmContent :: T.Text
    -- | DEPRECATED - Removed in version v2
    -- A list of File IDs that the message should use.
    -- There can be a maximum of 10 files attached to a message.
    -- Useful for tools like retrieval and code_interpreter that can access and use files.
  , thrmFileIds :: Maybe [FileId]
  , thrmAttachments :: Maybe [MessageAttachment]
  , thrmMetadata :: Maybe A.Value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 4) ''ThreadMessage)

data ThreadCreate = ThreadCreate
  { thrcMessages :: Maybe [ThreadMessage]
  , thrcToolResources :: Maybe ToolResources
  , thrcMetadata :: Maybe A.Value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 4) ''ThreadCreate)

newtype MessageId = MessageId {unMessageId :: T.Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData)
  deriving anyclass NFData

newtype ImageFile = ImageFile
  { imgfFileId :: FileId }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 4) ''ImageFile)

data TextMessage = TextMessage
  { tmsgValue :: T.Text
    -- FIXME(adn) Support annotations in the near future.
  , tmsgAnnotations :: [A.Value]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 4) ''TextMessage)

data MessageContent
  = MSG_image ImageFile
  | MSG_text  TextMessage
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

instance ToJSON MessageContent where
  toJSON = \case
    MSG_image imgFile
      -> A.object [ "type" A..= A.String "image_file", "image_file" A..= A.toJSON imgFile ]
    MSG_text txtMsg
      -> A.object [ "type" A..= A.String "text", "text" A..= A.toJSON txtMsg ]

instance FromJSON MessageContent where
  parseJSON = A.withObject "MessageContent" $ \o -> do
    ty <- o A..: "type"
    case ty of
      "image_file" -> do
        f <- o A..: "image_file"
        pure $ MSG_image f
      "text"       -> do
        t <- o A..: "text"
        pure $ MSG_text t
      _            -> A.typeMismatch ("MessageContent, invalid type: " <> show ty) ty

newtype RunId = RunId {unRunId :: T.Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData)
  deriving anyclass NFData

-- | A 'Message' object https://platform.openai.com/docs/api-reference/messages/object
data Message = Message
  { msgId          :: MessageId
  , msgObject      :: T.Text
  , msgCreatedAt   :: TimeStamp
  , msgThreadId    :: ThreadId
    -- | The entity that produced the message. One of 'user' or 'assistant'.
  , msgRole        :: T.Text
  , msgContent     :: [MessageContent]
  , msgAssistantId :: Maybe AssistantId
  , msgRunId       :: Maybe RunId
  -- | DEPRECATED removed in version v2
  , msgFileIds     :: Maybe [FileId]
  , msgAttachments :: Maybe [MessageAttachment]
  , msgMetadata    :: Maybe A.Value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 3) ''Message)

data RunStatus =
    RST_queued
  | RST_in_progress
  | RST_requires_action
  | RST_cancelling
  | RST_cancelled
  | RST_failed
  | RST_completed
  | RST_expired
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonEnumsOpts 4) ''RunStatus)

data RunErrorCode =
    REC_server_error
  | REC_rate_limit_exceeded
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonEnumsOpts 4) ''RunErrorCode)

data RunError = RunError
  { runeCode    :: RunErrorCode
  , runeMessage :: T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 4) ''RunError)

data SubmitToolOutputs = SubmitToolOutputs
  { stoToolCalls    :: [ChatToolCall]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 3) ''SubmitToolOutputs)

data RunRequiredAction = RunRequiredAction
  { rraType    :: T.Text
  , rraSubmitToolOutputs :: SubmitToolOutputs
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 3) ''RunRequiredAction)

-- | A 'Run' object: https://platform.openai.com/docs/api-reference/runs/object
data Run = Run
  { runId             :: RunId
  , runObject         :: T.Text
  , runCreatedAt      :: TimeStamp
  , runThreadId       :: ThreadId
  , runAssistantId    :: AssistantId
  , runStatus         :: RunStatus
  , runRequiredAction :: Maybe RunRequiredAction
  , runLastError      :: Maybe RunError
  , runStartedAt      :: Maybe TimeStamp
  , runExpiresAt      :: Maybe TimeStamp
  , runCancelledAt    :: Maybe TimeStamp
  , runFailedAt       :: Maybe TimeStamp
  , runCompletedAt    :: Maybe TimeStamp
  , runModel          :: ModelId
  , runInstructions   :: T.Text
  , runTools          :: [AssistantTool]
  , runFileIds        :: Maybe [FileId]
  , runMetadata       :: Maybe A.Value
  , runUsage          :: Maybe Usage
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 3) ''Run)

data RunCreate = RunCreate
  { rcrAssistantId            :: AssistantId
  , rcrModel                  :: Maybe ModelId
  , rcrInstructions           :: Maybe T.Text
  , rcrAdditionalInstructions :: Maybe T.Text
  , rcrAdditionalMessages     :: Maybe [ThreadMessage]
  , rcrTools                  :: Maybe [AssistantTool]
  , rcrMetadata               :: Maybe A.Value
  , rcrToolChoice             :: Maybe ChatToolChoiceStrategy
  , rcrParallelToolCalls      :: Maybe Bool
  , rcrResponseFormat         :: Maybe ResponseFormat
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 3) ''RunCreate)

data ToolOutput = ToolOutput
  { toToolCallId :: Maybe T.Text
  , toOutput     :: Maybe T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 2) ''ToolOutput)

data ToolOutputs = ToolOutputs
  { tosToolOutputs :: [ToolOutput]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 3) ''ToolOutputs)

-- | Create a thread and run it in one request.
data ThreadAndRunCreate = ThreadAndRunCreate
  { tarcAssistantId  :: AssistantId
  , tarcThread       :: Maybe ThreadCreate
  , tarcModel        :: Maybe ModelId
  , tarcInstructions :: Maybe T.Text
  , tarcTools        :: Maybe [AssistantTool]
  , tarcMetadata     :: Maybe A.Value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 4) ''ThreadAndRunCreate)

------------------------
------ Old stuff; not touching
------ TODO 2023.03.22: Not touching this; unchanged since last year
------------------------

newtype FineTuneId = FineTuneId {unFineTuneId :: T.Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData)
  deriving anyclass NFData

data FineTuneCreate = FineTuneCreate
  { ftcTrainingFile :: FileId,
    ftcValidationFile :: Maybe FileId,
    ftcModel :: Maybe T.Text,
    ftcBatchSize :: Maybe Int,
    ftcNEpochs :: Maybe T.Text,
    ftcLearningRateMultiplier :: Maybe Double,
    ftcPromptLossWeight :: Maybe Double,
    ftcComputeClassificationMetrics :: Maybe Bool,
    ftcClassificationNClasses :: Maybe Int,
    ftcClassificationPositiveClass :: Maybe T.Text
  }
  deriving (Show, Eq)

defaultFineTuneCreate :: FileId -> FineTuneCreate
defaultFineTuneCreate file =
  FineTuneCreate
    { ftcTrainingFile = file,
      ftcValidationFile = Nothing,
      ftcModel = Nothing,
      ftcBatchSize = Nothing,
      ftcNEpochs = Nothing,
      ftcLearningRateMultiplier = Nothing,
      ftcPromptLossWeight = Nothing,
      ftcComputeClassificationMetrics = Nothing,
      ftcClassificationNClasses = Nothing,
      ftcClassificationPositiveClass = Nothing
    }

data FineTuneEvent = FineTuneEvent
  { fteCreatedAt :: Int,
    fteLevel :: T.Text,
    fteMessage :: T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

data FineTune = FineTune
  { ftId :: FineTuneId,
    ftModel :: T.Text,
    ftCreatedAt :: Int,
    ftEvents :: V.Vector FineTuneEvent,
    ftTunedModel :: Maybe T.Text,
    ftStatus :: T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

data Order
  = ORD_asc
  | ORD_desc
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass NFData

instance ToHttpApiData Order where
  toQueryParam = \case
    ORD_asc  -> "asc"
    ORD_desc -> "desc"

instance FromHttpApiData Order where
  parseQueryParam "asc"  = Right ORD_asc
  parseQueryParam "desc" = Right ORD_desc
  parseQueryParam xs     = Left $ "Invalid sorting " <> xs <> ", allowed values: asc, desc"

$(deriveJSON (jsonOpts 3) ''FineTuneCreate)
$(deriveJSON (jsonOpts 3) ''FineTuneEvent)
$(deriveJSON (jsonOpts 2) ''FineTune)

data ChunkingStrategyType
  = CST_static
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass NFData

instance ToJSON ChunkingStrategyType where
  toJSON = \case
    CST_static -> A.String "static"

instance FromJSON ChunkingStrategyType where
  parseJSON (A.String "static") = pure CST_static
  parseJSON ty                  = A.typeMismatch ("ChunkingStrategyType, invalid type: " <> show ty) ty

data ChunkingStrategyStatic = ChunkingStrategyStatic
  { cssMaxChunkSizeTokens :: Int
  , cssChunkOverlapTokens :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

data ChunkingStrategy = ChunkingStrategy
  { cstType   :: ChunkingStrategyType
  , cstStatic :: ChunkingStrategyStatic
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

data ExpiresAfter = ExpiresAfter
  { exaAnchor :: T.Text
  , exaDays   :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

data VectorStoreFileCreate = VectorStoreFileCreate
  { vsfcFileIds :: Maybe [FileId]
  , vsfcExpiresAfter :: Maybe ExpiresAfter
  , vsfcName    :: T.Text
  , vsfcChunkingStrategy :: Maybe ChunkingStrategy
  , vsfcMetadata :: Maybe A.Value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

data VectorStoreCreate = VectorStoreCreate
  { vscFileIds :: Maybe [FileId]
  , vscExpiresAfter :: Maybe ExpiresAfter
  , vscName    :: T.Text
  , vscChunkingStrategy :: Maybe ChunkingStrategy
  , vscMetadata :: Maybe A.Value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

data VectorStoreStatus
  = VSS_expired
  | VSS_in_progress
  | VSS_completed
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass NFData

instance ToJSON VectorStoreStatus where
  toJSON = \case
    VSS_expired     -> A.String "expired"
    VSS_in_progress -> A.String "in_progress"
    VSS_completed   -> A.String "completed"

instance FromJSON VectorStoreStatus where
  parseJSON (A.String "expired")     = pure VSS_expired
  parseJSON (A.String "in_progress") = pure VSS_in_progress
  parseJSON (A.String "completed")   = pure VSS_completed
  parseJSON ty                       = A.typeMismatch ("VectorStoreStatus, invalid type: " <> show ty) ty

data VectorStore = VectorStore
  { vstId        :: VectorStoreId
  , vstObject    :: T.Text
  , vstCreatedAt :: TimeStamp
  , vstName      :: T.Text
  , vstExpiresAt :: Maybe TimeStamp
  , vstStatus    :: VectorStoreStatus
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

$(deriveJSON (jsonOpts 3) ''ChunkingStrategyStatic)
$(deriveJSON (jsonOpts 3) ''ChunkingStrategy)
$(deriveJSON (jsonOpts 3) ''VectorStore)
$(deriveJSON (jsonOpts 3) ''ExpiresAfter)
$(deriveJSON (jsonOpts 3) ''VectorStoreCreate)
