# Compaction API Support — Design

Tracks hermes ticket [iconnect/hermes#2573](https://github.com/iconnect/hermes/issues/2573).
Adds full support for OpenAI's Responses Compaction API (rolled out Feb 2026)
to the Haskell client. Two surfaces are covered:

1. **Server-side compaction** — `context_management` field on `POST /v1/responses`.
2. **Standalone compaction** — `POST /v1/responses/compact` endpoint plus the
   `compaction` item shape that appears in `Response.output`.

Schemas mirror the official `openai-python` SDK (`src/openai/types/responses/`).

## New types (`openai-api-servant/src/OpenAI/Resources.hs`)

### `ResponseCompactionItem`

Used both as an output item (returned by the API) and as an input echo item
(sent back to the model in the next request). The Python SDK splits these
(`ResponseCompactionItem` requires `id`; `ResponseCompactionItemParam` makes
`id` optional). One Haskell record handles both; `id` is `Maybe` so callers
can omit it on input.

```haskell
data ResponseCompactionItem = ResponseCompactionItem
  { rciId               :: Maybe T.Text  -- present on output, optional on input
  , rciEncryptedContent :: T.Text
  , rciCreatedBy        :: Maybe T.Text  -- output-only; null on input
  }
```

JSON: `{"id":..., "type":"compaction", "encrypted_content":..., "created_by":...}`.
`type` is injected by the enclosing `ResponseOutput` / `ResponseCreateInputItem`
discriminator code (same pattern as the other variants in this file).

### `ContextManagementItem`

```haskell
data ContextManagementItem
  = CMI_Compaction { cmiCompactThreshold :: Maybe Int }
```

JSON: `{"type":"compaction","compact_threshold":<int|absent>}`.
Modelled as an ADT (not a record + free-form `type`) so adding future
strategies stays exhaustive at the call site.

### `ResponseCompactCreate`

Body of `POST /v1/responses/compact`. Mirrors `ResponseCompactParams`:

```haskell
data ResponseCompactCreate = ResponseCompactCreate
  { rccModel                :: ModelId
  , rccInput                :: Maybe ResponseInput
  , rccInstructions         :: Maybe T.Text
  , rccPreviousResponseId   :: Maybe ResponseId
  , rccPromptCacheKey       :: Maybe T.Text
  , rccPromptCacheRetention :: Maybe PromptCacheRetention
  , rccServiceTier          :: Maybe ResponseServiceTier  -- reuse existing
  }
```

### `PromptCacheRetention`

```haskell
data PromptCacheRetention = PCR_InMemory | PCR_24h
```

JSON: `"in_memory"` / `"24h"`.

### `CompactedResponse`

Return shape of `POST /v1/responses/compact`. Distinct from `Response` —
`object` is fixed `"response.compaction"` and there's no `error`/`status`/etc.

```haskell
data CompactedResponse = CompactedResponse
  { crId        :: T.Text
  , crCreatedAt :: Int
  , crObject    :: T.Text          -- always "response.compaction"
  , crOutput    :: [ResponseOutput]
  , crUsage     :: ResponseUsage
  }
```

## Modified types

### `ResponseCreate`

Add one field:

```haskell
, recrContextManagement :: Maybe [ContextManagementItem]
```

JSON field name: `context_management`.

### `ResponseOutput`

Add variant `RO_Compaction ResponseCompactionItem`. Update both
`FromJSON` and `ToJSON` instances to handle `type == "compaction"`.

### `ResponseCreateInputItem`

Add variant `RII_Compaction ResponseCompactionItem` so callers can echo
the compaction item back on the next turn.

## Servant API (`openai-api-servant/src/OpenAI/Api.hs`)

Extend `ResponsesApi`:

```haskell
:<|> OpenAIAuth
  :> "compact"
  :> ReqBody '[JSON] ResponseCompactCreate
  :> Post '[JSON] CompactedResponse
```

## Client (`openai-api/src/OpenAI/Client.hs`)

- Re-export `ResponseCompactionItem`, `ContextManagementItem`,
  `ResponseCompactCreate`, `PromptCacheRetention`, `CompactedResponse`.
- Add `compactResponse :: MonadIO m => OpenAIClient -> ResponseCompactCreate
  -> m (Either ClientError CompactedResponse)` via the existing `EP1` macro.
- Thread the new client into the `:<|>` chain alongside `createResponse`,
  `getResponse`, `deleteResponse`, `getResponseInputItems`.

## Tests (`openai-api/test/ApiSpec.hs`)

JSON round-trip / golden checks:

- `ContextManagementItem` with and without `compact_threshold`.
- `ResponseCompactionItem` decode (output shape with id + created_by) and
  encode (input shape; absent `id` round-trips through `Nothing`).
- `ResponseOutput` decode for `{"type":"compaction", ...}`.
- `ResponseCompactCreate` minimal (only model) and full payload.
- `CompactedResponse` decode of a representative payload.
- `ResponseCreate` round-trip including `context_management`.

These are pure JSON tests in the existing `Spec` harness — no network calls.

## Out of scope

- Streaming compaction events (separate ticket if needed).
- Automatic re-compaction loops on the consumer side (hermes-side concern).
- Conversation/model enum exhaustiveness — `ModelId` stays free-form.
