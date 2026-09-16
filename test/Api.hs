{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{- swiped from an incomplete personal project to use for testing. -}
module Api (
  -- * Api structure
  Api(..),
  ProtectedApi(..),
  UnprotectedApi(..),

  -- * Api data
  ProposalId(..),
  AvailabilityInterval(..),
  NewProposalReq(..),
  Name(..),
  Invite(..),
  Interval(..),
  Token(..),
  Email(..),
  Availability(..),
  DiscordAccessToken(..),
  DiscordUser(..),
  Guild(..),
  GuildId(..),
  AvailableCredits(..),
  DashboardData(..),
  Proposal(..),
  Cookie(..),
  SetMetadataReq(..),
  KV(..),
  FEConfig(..),
  Guilds(..),
) where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.JsonSpec
  ( HasJsonDecodingSpec(DecodingSpec), HasJsonEncodingSpec(EncodingSpec)
  , Module(Module)
  , Specification
    ( JsonArray, JsonDateTime, JsonDict, JsonEither, JsonInt, JsonLet
    , JsonModule, JsonObject, JsonRef, JsonString, JsonTag
    )
  , type (:::), type (::?), type (:=)
  )
import Data.JsonSpec.Codec.Tuple
  ( Field(Field), SpecJson(SpecJson), TupleDecoding(fromJsonStructure)
  , TupleEncoding(toJsonStructure)
  )
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Prelude
  ( Applicative(pure), Either(Right), (.), Eq, Int, Maybe, Ord, undefined
  )
import Servant.API
  ( FromHttpApiData(parseHeader, parseQueryParam), GenericMode((:-))
  , StdMethod(GET), (:>), Capture, DeleteNoContent, Get, Header, Header'
  , Headers, JSON, NamedRoutes, NoContent, Optional, Post, PostNoContent
  , QueryParam', ReqBody, ReqBody', Required, Strict, Summary, ToHttpApiData
  , Verb
  )
import Web.Cookie (SetCookie)

data Api mode = Api
  { protectedApi :: mode
      :- "api"
      :> Header' '[Optional, Strict] "Authorization" Token
      :> Header' '[Optional, Strict] "Cookie" Cookie
      :> NamedRoutes ProtectedApi
  , unprotectedApi :: mode
      :- "api"
      :> NamedRoutes UnprotectedApi
  }
  deriving stock (Generic)


newtype Cookie = Cookie ByteString
instance FromHttpApiData Cookie where
  parseHeader = Right . Cookie
  parseQueryParam = Right . Cookie . encodeUtf8


data ProtectedApi mode = ProtectedApi
  { makeProposal :: mode
      :- "proposal"
      :> Summary "a summary"
      :> ReqBody' '[Required, Strict] '[JSON] NewProposalReq
      :> Post '[JSON] (KV ProposalId Proposal)

  , deleteProposal :: mode
      :- "proposal"
      :> Capture "proposalId" ProposalId
      :> DeleteNoContent

  , setAvailability :: mode
      :- "proposal"
      :> Capture "proposalId" ProposalId
      :> "availability"
      :> ReqBody' '[Required, Strict] '[JSON] Availability
      :> PostNoContent

  , setMetadata :: mode
      :- "proposal"
      :> Capture "proposalId" ProposalId
      :> "metadata"
      :> ReqBody' '[Required, Strict] '[JSON] SetMetadataReq
      :> PostNoContent

  , dashboard :: mode
      :- "dashboard"
      :> QueryParam' '[Required, Strict] "foo" Text
      :> QueryParam' '[Optional, Strict] "bar" Text
      :> Get '[JSON] DashboardData

  , addInvite :: mode
      :- "proposal"
      :> Capture "proposalId" ProposalId
      :> "invites"
      :> "add-invite"
      :> ReqBody' '[Required, Strict] '[JSON] Invite
      :> PostNoContent

  , deleteInvite :: mode
      :- "proposal"
      :> Capture "proposalId" ProposalId
      :> "invites"
      :> "delete"
      :> ReqBody' '[Required, Strict] '[JSON] Invite
      :> PostNoContent

  , getGuilds :: mode
      :- "guilds"
      :> Get '[JSON] Guilds

  }
  deriving stock (Generic)


newtype Guilds = Guilds
  { unGuilds :: Set Guild
  }
  deriving ToJSON via (SpecJson Guilds)
instance HasJsonEncodingSpec Guilds where
  type EncodingSpec Guilds =
    'Module (JsonArray (JsonModule (EncodingSpec Guild)))
instance TupleEncoding Guilds where
  toJsonStructure = undefined


data SetMetadataReq = SetMetadataReq
  {        name :: Name
  , description :: Text
  ,       venue :: Text
  }
  deriving FromJSON via (SpecJson SetMetadataReq)
instance HasJsonDecodingSpec SetMetadataReq where
  type DecodingSpec SetMetadataReq =
    'Module
      (JsonObject
        '[        "name" ::: JsonModule (DecodingSpec Name)
         , "description" ::: JsonString
         ,       "venue" ::: JsonString
         ])
instance TupleDecoding SetMetadataReq where
  fromJsonStructure
      (Field @"name" name_,
      (Field @"description" description,
      (Field @"venue" venue,
      ())))
    = do
      name <- fromJsonStructure name_
      pure
        SetMetadataReq
          { name
          , description
          , venue
          }


data DashboardData = DashboardData
  { proposals :: Map ProposalId Proposal
  ,   credits :: AvailableCredits
  ,      user :: DiscordUser
  }
  deriving ToJSON via (SpecJson DashboardData)
instance HasJsonEncodingSpec DashboardData where
  type EncodingSpec DashboardData =
    'Module
      (JsonLet
        '[ "DashboardData" :=
             JsonObject
               '[ "proposals" ::: JsonDict (JsonModule (EncodingSpec Proposal))
                , "credits" ::: JsonModule (EncodingSpec AvailableCredits)
                , "user" ::: JsonModule (EncodingSpec DiscordUser)
                ]
         ]
        (JsonRef "DashboardData"))
instance TupleEncoding DashboardData where
  toJsonStructure = undefined


data Proposal = Proposal
  {         name :: Name
  ,        owner :: DiscordUser
  ,  description :: Text
  ,        venue :: Text
  , availability :: [AvailabilityInterval]
  ,      invites :: Set Invite
  ,    createdAt :: UTCTime
  }
  deriving stock (Generic)
  deriving (ToJSON, FromJSON) via (SpecJson Proposal)
instance HasJsonEncodingSpec Proposal where
  type EncodingSpec Proposal =
    'Module
      (JsonObject
        '[         "name" ::: JsonModule (EncodingSpec Name)
         ,        "owner" ::: JsonModule (EncodingSpec DiscordUser)
         , "availability" ::: JsonArray (JsonModule (EncodingSpec AvailabilityInterval))
         ,  "description" ::: JsonString
         ,        "venue" ::: JsonString
         ,      "invites" ::: JsonArray (JsonModule (EncodingSpec Invite))
         ,   "created-at" ::: JsonDateTime
         ])
instance TupleEncoding Proposal where
  toJsonStructure = undefined
instance HasJsonDecodingSpec Proposal where
  type DecodingSpec Proposal = EncodingSpec Proposal
instance TupleDecoding Proposal where
  fromJsonStructure = undefined


data Invite
  = InviteUser DiscordUser
  | InviteGuild Guild
  deriving stock (Eq, Ord)
  deriving (ToJSON, FromJSON) via (SpecJson Invite)
instance HasJsonEncodingSpec Invite where
  type EncodingSpec Invite =
    'Module
      (JsonLet
        '[ "Invite" :=
             JsonEither
               '[ JsonObject
                    '[     "type" ::: JsonTag "discord-user"
                     , "username" ::: JsonModule (EncodingSpec DiscordUser)
                     ]
                , JsonObject
                    '[  "type" ::: JsonTag "discord-server"
                     , "guild" ::: JsonModule (EncodingSpec Guild)
                     ]
                ]
         ]
        (JsonRef "Invite"))
instance TupleEncoding Invite where
  toJsonStructure = undefined
instance HasJsonDecodingSpec Invite where
  type DecodingSpec Invite = EncodingSpec Invite
instance TupleDecoding Invite where
  fromJsonStructure = undefined


data Guild = Guild
  { guildId :: GuildId
  ,    name :: Text
  }
  deriving stock (Eq, Ord)
  deriving (ToJSON, FromJSON) via (SpecJson Guild)
instance HasJsonEncodingSpec Guild where
  type EncodingSpec Guild =
    'Module
      (JsonObject
        '[   "id" ::: JsonModule (EncodingSpec GuildId)
         , "name" ::: JsonString
         ])
instance TupleEncoding Guild where
  toJsonStructure = undefined
instance HasJsonDecodingSpec Guild where
  type DecodingSpec Guild = EncodingSpec Guild
instance TupleDecoding Guild where
  fromJsonStructure = undefined


newtype GuildId = GuildId
  { unGuildId :: Text
  }
  deriving newtype (ToHttpApiData, Eq, Ord)
  deriving FromJSON via (SpecJson GuildId)
instance HasJsonEncodingSpec GuildId where
  type EncodingSpec GuildId = 'Module JsonString
instance TupleEncoding GuildId where
  toJsonStructure = unGuildId
instance HasJsonDecodingSpec GuildId where
  type DecodingSpec GuildId = EncodingSpec GuildId
instance TupleDecoding GuildId where
  fromJsonStructure = pure . GuildId


data AvailabilityInterval = AvailabilityInterval
  { interval :: Interval
  ,    users :: Set DiscordUser
  }
  deriving ToJSON via (SpecJson AvailabilityInterval)
instance HasJsonDecodingSpec AvailabilityInterval where
  type DecodingSpec AvailabilityInterval = EncodingSpec AvailabilityInterval
instance TupleDecoding AvailabilityInterval where
  fromJsonStructure = undefined
instance HasJsonEncodingSpec AvailabilityInterval where
  type EncodingSpec AvailabilityInterval =
    'Module
      (JsonObject
        '[ "interval" ::: JsonModule (EncodingSpec Interval)
         ,    "users" ::: JsonArray (JsonModule (EncodingSpec DiscordUser))
         ])
instance TupleEncoding AvailabilityInterval where
  toJsonStructure = undefined


newtype AvailableCredits = AvailableCredits
  { unAvailableCredits :: Int
  }
  deriving ToJSON via (SpecJson AvailableCredits)
instance HasJsonEncodingSpec AvailableCredits where
  type EncodingSpec AvailableCredits = 'Module JsonInt
instance TupleEncoding AvailableCredits where
  toJsonStructure = undefined


data UnprotectedApi mode = UnprotectedApi
  { login :: mode
      :- "login"
      :> ReqBody '[JSON] DiscordAccessToken
      :> Post
          '[JSON]
          (
            Headers
              '[ Header "Set-Cookie" SetCookie
               , Header "Set-Cookie" SetCookie
               ]
              DiscordUser
          )
  , logout :: mode
      :- "logout"
      :> Verb 'GET 204 '[JSON]
          (
            Headers
              '[ Header "Set-Cookie" SetCookie
               , Header "Set-Cookie" SetCookie
               ]
              NoContent
          )
  , config :: mode
      :- "config"
      :> Get '[JSON] FEConfig
  }
  deriving stock (Generic)


newtype FEConfig = FEConfig
  { discordRedirect :: Text
  }
  deriving (ToJSON) via (SpecJson FEConfig)
instance HasJsonEncodingSpec FEConfig where
  type EncodingSpec FEConfig =
    'Module
      (JsonObject
        '[ "redirectUrl" ::: JsonString ])
instance TupleEncoding FEConfig where
  toJsonStructure = undefined

newtype Email = Email
  { unEmail :: Text
  }
  deriving newtype (FromJSON)


newtype DiscordAccessToken = DiscordAccessToken
  { unDiscordAccessToken :: Text
  }
  deriving newtype (Binary)
  deriving (FromJSON) via (SpecJson DiscordAccessToken)
instance HasJsonDecodingSpec DiscordAccessToken where
  type DecodingSpec DiscordAccessToken = 'Module JsonString
instance TupleDecoding DiscordAccessToken where
  fromJsonStructure = undefined


newtype Token = Token
  { unToken :: Text
  }
  deriving newtype
    ( Eq
    , FromHttpApiData
    , Ord
    )


newtype ProposalId = ProposalId
  { unProposalId :: UUID
  }
  deriving newtype
    ( Eq
    , FromHttpApiData
    , Ord
    , ToJSONKey
    , FromJSONKey
    )
  deriving ToJSON via (SpecJson ProposalId)
instance HasJsonEncodingSpec ProposalId where
  type EncodingSpec ProposalId = 'Module JsonString
instance TupleEncoding ProposalId where
  toJsonStructure = undefined


data NewProposalReq = NewProposalReq
  {         name :: Name
  , availability :: Availability
  ,  description :: Text
  ,        venue :: Maybe Text
  }
  deriving (FromJSON) via (SpecJson NewProposalReq)
instance HasJsonDecodingSpec NewProposalReq where
  type DecodingSpec NewProposalReq =
    'Module
      (JsonObject
        '[         "name" ::: JsonModule (DecodingSpec Name)
         , "availability" ::: JsonModule (DecodingSpec Availability)
         ,  "description" ::: JsonString
         ,        "venue" ::? JsonString
         ])
instance TupleDecoding NewProposalReq where
  fromJsonStructure = undefined


newtype Availability = Availability
  { unAvailability :: Set Interval
  }
  deriving FromJSON via (SpecJson Availability)
instance HasJsonDecodingSpec Availability where
  type DecodingSpec Availability =
    'Module (JsonArray (JsonModule (DecodingSpec Interval)))
instance TupleDecoding Availability where
  fromJsonStructure = undefined


newtype Name = Name
  { unName :: Text
  }
  deriving (ToJSON, FromJSON) via (SpecJson Name)
instance HasJsonEncodingSpec Name where
  type EncodingSpec Name = 'Module JsonString
instance TupleEncoding Name where
  toJsonStructure = undefined
instance HasJsonDecodingSpec Name where
  type DecodingSpec Name = EncodingSpec Name
instance TupleDecoding Name where
  fromJsonStructure = undefined


data Interval = Interval
  { startInclusive :: UTCTime
  ,   endExclusive :: UTCTime
  }
  deriving stock (Eq, Ord)
  deriving (ToJSON, FromJSON) via (SpecJson Interval)
instance HasJsonEncodingSpec Interval where
  type EncodingSpec Interval =
    'Module
      (JsonObject
        '[ "startInclusive" ::: JsonDateTime
         ,   "endExclusive" ::: JsonDateTime
         ])
instance TupleEncoding Interval where
  toJsonStructure = undefined
instance HasJsonDecodingSpec Interval where
  type DecodingSpec Interval = EncodingSpec Interval
instance TupleDecoding Interval where
  fromJsonStructure = undefined


newtype DiscordUser = DiscordUser
  { unDiscordUser :: Text
  }
  deriving newtype ( Eq , FromJSON , Ord, Binary)
  deriving ToJSON via (SpecJson DiscordUser)
instance HasJsonEncodingSpec DiscordUser where
  type EncodingSpec DiscordUser = 'Module JsonString
instance TupleEncoding DiscordUser where
  toJsonStructure = undefined
instance HasJsonDecodingSpec DiscordUser where
  type DecodingSpec DiscordUser = EncodingSpec DiscordUser
instance TupleDecoding DiscordUser where
  fromJsonStructure = undefined


data KV k v = KV
  {   key :: k
  , value :: v
  }
deriving via (SpecJson (KV ProposalId Proposal)) instance
  ToJSON (KV ProposalId Proposal)
instance HasJsonEncodingSpec (KV k v) where
  type EncodingSpec (KV k v) =
    'Module
      (JsonObject
        '[   "key" ::: JsonModule (EncodingSpec k)
         , "value" ::: JsonModule (EncodingSpec v)
         ])
instance TupleEncoding (KV k v) where
  toJsonStructure = undefined
