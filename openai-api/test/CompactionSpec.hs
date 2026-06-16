module CompactionSpec (compactionSpec) where

import Data.Aeson
  ( FromJSON
  , ToJSON
  , Value (..)
  , decode
  , eitherDecode
  , encode
  , object
  , toJSON
  , (.=)
  )
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import OpenAI.Client
import Test.Hspec

roundtrip :: (Eq a, Show a, ToJSON a, FromJSON a) => a -> Expectation
roundtrip x = decode (encode x) `shouldBe` Just x

compactionSpec :: Spec
compactionSpec = describe "compaction api" $ do
  describe "ContextManagementItem" $ do
    it "encodes with compact_threshold" $
      toJSON (CMI_Compaction (Just 200000))
        `shouldBe` object [ "type" .= ("compaction" :: T.Text)
                          , "compact_threshold" .= (200000 :: Int)
                          ]
    it "encodes without compact_threshold" $
      toJSON (CMI_Compaction Nothing)
        `shouldBe` object [ "type" .= ("compaction" :: T.Text) ]
    it "round-trips" $ do
      roundtrip (CMI_Compaction (Just 200000))
      roundtrip (CMI_Compaction Nothing)
    it "rejects unknown type" $
      (eitherDecode "{\"type\":\"truncation\"}" :: Either String ContextManagementItem)
        `shouldSatisfy` either (const True) (const False)

  describe "PromptCacheRetention" $ do
    it "encodes in_memory" $ toJSON PCR_InMemory `shouldBe` String "in_memory"
    it "encodes 24h"       $ toJSON PCR_24h      `shouldBe` String "24h"
    it "round-trips" $ do
      roundtrip PCR_InMemory
      roundtrip PCR_24h

  describe "ResponseCompactionItem" $ do
    it "decodes the output shape" $ do
      let raw = "{\"id\":\"cmp_123\",\"encrypted_content\":\"abc==\",\"created_by\":\"resp_456\"}"
      (decode raw :: Maybe ResponseCompactionItem)
        `shouldBe` Just (ResponseCompactionItem (Just "cmp_123") "abc==" (Just "resp_456"))
    it "round-trips without id (input echo shape)" $
      roundtrip (ResponseCompactionItem Nothing "abc==" Nothing)
    it "omits Nothing fields" $
      toJSON (ResponseCompactionItem Nothing "abc==" Nothing)
        `shouldBe` object ["encrypted_content" .= ("abc==" :: T.Text)]

  describe "ResponseOutput compaction variant" $ do
    it "decodes type=compaction" $ do
      let raw =
            "{\"type\":\"compaction\",\"id\":\"cmp_1\",\"encrypted_content\":\"x\"}"
      (decode raw :: Maybe ResponseOutput)
        `shouldBe` Just (RO_Compaction (ResponseCompactionItem (Just "cmp_1") "x" Nothing))
    it "encodes injecting the type tag" $
      let v = toJSON (RO_Compaction (ResponseCompactionItem (Just "cmp_1") "x" Nothing))
      in case v of
           Object o ->
             KM.lookup "type" o `shouldBe` Just (String "compaction")
           _ -> expectationFailure "expected object"

  describe "ResponseCreateInputItem compaction variant" $
    it "round-trips" $
      roundtrip (RII_Compaction (ResponseCompactionItem (Just "cmp_1") "x" Nothing))

  describe "ResponseCompactCreate" $ do
    it "encodes minimal payload (model only)" $
      let req = ResponseCompactCreate
                  (ModelId "gpt-5.4")
                  Nothing Nothing Nothing Nothing Nothing Nothing
          enc = toJSON req
      in case enc of
           Object o -> do
             KM.lookup "model" o `shouldBe` Just (String "gpt-5.4")
             KM.member "input" o `shouldBe` False
             KM.member "instructions" o `shouldBe` False
           _ -> expectationFailure "expected object"
    it "round-trips full payload" $ do
      let req = ResponseCompactCreate
                  (ModelId "gpt-5.4")
                  (Just (RI_Text "hello"))
                  (Just "be terse")
                  (Just (ResponseId "resp_42"))
                  (Just "cache-key")
                  (Just PCR_24h)
                  (Just RST_default)
      decode (encode req) `shouldBe` Just req

  describe "CompactedResponse" $
    it "decodes a representative payload" $ do
      let raw = "{\"id\":\"cmp_1\",\"created_at\":1700000000,\"object\":\"response.compaction\",\"output\":[{\"type\":\"compaction\",\"id\":\"item_1\",\"encrypted_content\":\"E==\"}],\"usage\":{\"input_tokens\":10,\"input_tokens_details\":{},\"output_tokens\":5,\"output_tokens_details\":{},\"total_tokens\":15}}"
      case eitherDecode raw :: Either String CompactedResponse of
        Left e  -> expectationFailure e
        Right r -> do
          cmprId r `shouldBe` "cmp_1"
          cmprCreatedAt r `shouldBe` 1700000000
          cmprObject r `shouldBe` "response.compaction"
          cmprOutput r `shouldBe`
            [RO_Compaction (ResponseCompactionItem (Just "item_1") "E==" Nothing)]

  describe "ResponseOutput extra variants" $ do
    it "decodes function_call_output" $ do
      let raw = "{\"type\":\"function_call_output\",\"call_id\":\"call_1\",\"output\":\"42\"}"
      case decode raw :: Maybe ResponseOutput of
        Just (RO_FunctionCallOutput _) -> pure ()
        other -> expectationFailure ("expected RO_FunctionCallOutput, got " <> show other)
    it "decodes mcp_call" $ do
      let raw = "{\"type\":\"mcp_call\",\"id\":\"m1\",\"arguments\":\"{}\",\"name\":\"do\",\"server_label\":\"srv\"}"
      case decode raw :: Maybe ResponseOutput of
        Just (RO_McpCall _) -> pure ()
        other -> expectationFailure ("expected RO_McpCall, got " <> show other)
    it "decodes image_generation_call" $ do
      let raw = "{\"type\":\"image_generation_call\",\"id\":\"img_1\",\"result\":null,\"status\":\"completed\"}"
      case decode raw :: Maybe ResponseOutput of
        Just (RO_ImageGenerationCall _) -> pure ()
        other -> expectationFailure ("expected RO_ImageGenerationCall, got " <> show other)
    it "falls back to RO_Unknown for unknown discriminators and round-trips verbatim" $ do
      let raw = "{\"type\":\"some_future_call\",\"id\":\"x\",\"weird_field\":[1,2,3]}"
          orig = decode raw :: Maybe Value
      case decode raw :: Maybe ResponseOutput of
        Just (RO_Unknown v) -> Just v `shouldBe` orig
        other -> expectationFailure ("expected RO_Unknown, got " <> show other)

  describe "ResponseCreateInputItem extra variants" $
    it "round-trips an RII_Unknown payload" $ do
      let raw = "{\"type\":\"future_input\",\"id\":\"x\",\"data\":42}"
          orig = decode raw :: Maybe Value
      case decode raw :: Maybe ResponseCreateInputItem of
        Just (RII_Unknown v) -> Just v `shouldBe` orig
        other                -> expectationFailure ("expected RII_Unknown, got " <> show other)

  describe "ResponseCreate context_management" $
    it "round-trips with a compaction entry" $ do
      let req = ResponseCreate
                  { recrModel              = ModelId "gpt-5.4"
                  , recrInput              = RI_Text "hi"
                  , recrInclude            = Nothing
                  , recrInstructions       = Nothing
                  , recrMaxOutputTokens    = Nothing
                  , recrMetadata           = Nothing
                  , recrParallelToolCalls  = Nothing
                  , recrPreviousResponseId = Nothing
                  , recrReasoning          = Nothing
                  , recrServiceTier        = Nothing
                  , recrStore              = Just False
                  , recrStream             = Nothing
                  , recrTemperature        = Nothing
                  , recrTopP               = Nothing
                  , recrTruncation         = Nothing
                  , recrUser               = Nothing
                  , recrText               = Nothing
                  , recrToolChoice         = Nothing
                  , recrTools              = Nothing
                  , recrContextManagement  = Just [CMI_Compaction (Just 200000)]
                  }
      decode (encode req) `shouldBe` Just req
