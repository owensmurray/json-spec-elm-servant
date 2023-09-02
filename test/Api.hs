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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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
) where


import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.JsonSpec (Field(Field), HasJsonDecodingSpec(DecodingSpec,
  fromJSONStructure), HasJsonEncodingSpec(EncodingSpec, toJSONStructure),
  SpecJSON(SpecJSON), Specification(JsonArray, JsonDateTime, JsonEither,
  JsonInt, JsonLet, JsonObject, JsonRef, JsonString, JsonTag), Tag(Tag))
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Prelude (Applicative(pure), Either(Left, Right), Functor(fmap),
  Traversable(traverse), (.), (<$>), Eq, Int, Ord)
import Servant.API (FromHttpApiData(parseHeader, parseQueryParam),
  GenericMode((:-)), StdMethod(GET), (:>), Capture, DeleteNoContent, Get,
  Header, Header', Headers, JSON, NamedRoutes, NoContent, Optional, Post,
  PostNoContent, ReqBody, ReqBody', Required, Strict, ToHttpApiData, Verb)
import Web.Cookie (SetCookie)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.UUID as UUID


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
      :> Get '[JSON] DashboardData

  , addInvite :: mode
      :- "proposal"
      :> Capture "proposalId" ProposalId
      :> "invites"
      :> "add"
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
      :> Get '[JSON] (Set Guild)

  }
  deriving stock (Generic)


data SetMetadataReq = SetMetadataReq
  {        name :: Name
  , description :: Text
  ,       venue :: Text
  }
  deriving FromJSON via (SpecJSON SetMetadataReq)
instance HasJsonDecodingSpec SetMetadataReq where
  type DecodingSpec SetMetadataReq =
    JsonObject
      '[ '("name", DecodingSpec Name)
       , '("description", JsonString)
       , '("venue", JsonString)
       ]
  fromJSONStructure
      (Field @"name" name_,
      (Field @"description" description,
      (Field @"venue" venue,
      ())))
    = do
      name <- fromJSONStructure name_
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
  deriving ToJSON via (SpecJSON DashboardData)
instance HasJsonEncodingSpec DashboardData where
  type EncodingSpec DashboardData =
    JsonLet
      '[ '("DashboardData"
          , JsonObject
              '[ '("proposals",
                    JsonArray
                      ( JsonObject
                          '[ '("key", EncodingSpec ProposalId)
                           , '("value", EncodingSpec Proposal)
                           ]
                      )
                  )
               , '("credits", EncodingSpec AvailableCredits)
               , '("user", EncodingSpec DiscordUser)
               ]
          )
       ]
       (JsonRef "DashboardData")
  toJSONStructure DashboardData {proposals, credits, user} =
    (Field @"proposals"
      [ (Field @"key" (toJSONStructure k),
        (Field @"value" (toJSONStructure v),
        ()))
      | (k, v) <- Map.toAscList proposals
      ],
    (Field @"credits" (toJSONStructure credits),
    (Field @"user" (toJSONStructure user),
    ())))


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
  deriving (ToJSON, FromJSON) via (SpecJSON Proposal)
instance HasJsonEncodingSpec Proposal where
  type EncodingSpec Proposal =
    JsonObject
      '[ '("name", EncodingSpec Name)
       , '("owner", EncodingSpec DiscordUser)
       , '("availability", JsonArray (EncodingSpec AvailabilityInterval))
       , '("description", JsonString)
       , '("venue", JsonString)
       , '("invites", JsonArray (EncodingSpec Invite))
       , '("created-at", JsonDateTime)
       ]
  toJSONStructure
      Proposal
        { name
        , owner
        , availability
        , description
        , invites
        , venue
        , createdAt
        }
    =
      (Field @"name" (toJSONStructure name),
      (Field @"owner" (toJSONStructure owner),
      (Field @"availability" (toJSONStructure <$> availability),
      (Field @"description" description,
      (Field @"venue" venue,
      (Field @"invites" (toJSONStructure <$> Set.toList invites),
      (Field @"created-at" createdAt,
      ())))))))
instance HasJsonDecodingSpec Proposal where
  type DecodingSpec Proposal = EncodingSpec Proposal
  fromJSONStructure
      (Field @"name" name,
      (Field @"owner" user,
      (Field @"availability" rawAvailability,
      (Field @"description" description,
      (Field @"venue" venue,
      (Field @"invites" rawInvites,
      (Field @"created-at" createdAt,
      ())))))))
    = do
      availability <- traverse fromJSONStructure rawAvailability
      invites <- traverse fromJSONStructure rawInvites
      pure
        Proposal
          { name = Name name
          , owner = DiscordUser user
          , description
          , availability
          , invites = Set.fromList invites
          , venue
          , createdAt
          }


data Invite
  = InviteUser DiscordUser
  | InviteGuild Guild
  deriving stock (Eq, Ord)
  deriving (ToJSON, FromJSON) via (SpecJSON Invite)
instance HasJsonEncodingSpec Invite where
  type EncodingSpec Invite =
    JsonLet
      '[ '( "Invite"
          , JsonEither
              ( JsonObject
                  '[ '("type", JsonTag "discord-user")
                   , '("username", EncodingSpec DiscordUser)
                   ]
              )
              ( JsonObject
                  '[ '("type", JsonTag "discord-server")
                   , '("guild", EncodingSpec Guild)
                   ]
              )
          )
       ]
       (JsonRef "Invite")
  toJSONStructure = \case
    InviteUser user ->
      Left
        (Field @"type" (Tag @"discord-user"),
        (Field @"username" (toJSONStructure user),
        ()))
    InviteGuild guild ->
      Right
        (Field @"type" (Tag @"discord-server"),
        (Field @"guild" (toJSONStructure guild),
        ()))
instance HasJsonDecodingSpec Invite where
  type DecodingSpec Invite = EncodingSpec Invite
  fromJSONStructure = \case
    Left
        (Field @"type" (Tag @"discord-user"),
        (Field @"username" user,
        ()))
      ->
        InviteUser <$> fromJSONStructure user

    Right
        (Field @"type" (Tag @"discord-server"),
        (Field @"guild" guild,
        ()))
      ->
        InviteGuild <$> fromJSONStructure guild


data Guild = Guild
  { guildId :: GuildId
  ,    name :: Text
  }
  deriving stock (Eq, Ord)
  deriving (ToJSON, FromJSON) via (SpecJSON Guild)
instance HasJsonEncodingSpec Guild where
  type EncodingSpec Guild =
    JsonObject
      '[ '("id", EncodingSpec GuildId)
       , '("name", JsonString)
       ]
  toJSONStructure Guild { guildId , name } =
    (Field @"id" (toJSONStructure guildId),
    (Field @"name" name,
    ()))
instance HasJsonDecodingSpec Guild where
  type DecodingSpec Guild = EncodingSpec Guild
  fromJSONStructure
      (Field @"id" id_,
      (Field @"name" name,
      ()))
    = do
      guildId <- fromJSONStructure id_
      pure Guild { guildId , name }


newtype GuildId = GuildId
  { unGuildId :: Text
  }
  deriving newtype (ToHttpApiData, Eq, Ord)
  deriving FromJSON via (SpecJSON GuildId)
instance HasJsonEncodingSpec GuildId where
  type EncodingSpec GuildId = JsonString
  toJSONStructure = unGuildId
instance HasJsonDecodingSpec GuildId where
  type DecodingSpec GuildId = EncodingSpec GuildId
  fromJSONStructure = pure . GuildId


data AvailabilityInterval = AvailabilityInterval
  { interval :: Interval
  ,    users :: Set DiscordUser
  }
  deriving ToJSON via (SpecJSON AvailabilityInterval)
instance HasJsonDecodingSpec AvailabilityInterval where
  type DecodingSpec AvailabilityInterval = EncodingSpec AvailabilityInterval
  fromJSONStructure
      (Field @"interval" rawInterval,
      (Field @"users" rawUsers,
      ()))
    = do
      interval <- fromJSONStructure rawInterval
      pure
        AvailabilityInterval
          { interval
          , users = Set.fromList (DiscordUser <$> rawUsers)
          }
instance HasJsonEncodingSpec AvailabilityInterval where
  type EncodingSpec AvailabilityInterval =
    JsonObject '[
      '("interval", EncodingSpec Interval),
      '("users", JsonArray (EncodingSpec DiscordUser))
    ]
  toJSONStructure AvailabilityInterval { interval , users } =
    (Field @"interval" (toJSONStructure interval),
    (Field @"users" (toJSONStructure <$> Set.toList users),
    ()))


newtype AvailableCredits = AvailableCredits
  { unAvailableCredits :: Int
  }
  deriving ToJSON via (SpecJSON AvailableCredits)
instance HasJsonEncodingSpec AvailableCredits where
  type EncodingSpec AvailableCredits = JsonInt
  toJSONStructure = unAvailableCredits


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
  deriving (ToJSON) via (SpecJSON FEConfig)
instance HasJsonEncodingSpec FEConfig where
  type EncodingSpec FEConfig =
    JsonObject
      '[ '( "redirectUrl", JsonString) ]
  toJSONStructure FEConfig { discordRedirect } =
    (Field @"redirectUrl" discordRedirect,
    ())

newtype Email = Email
  { unEmail :: Text
  }
  deriving newtype (FromJSON)


newtype DiscordAccessToken = DiscordAccessToken
  { unDiscordAccessToken :: Text
  }
  deriving newtype (Binary)
  deriving (FromJSON) via (SpecJSON DiscordAccessToken)
instance HasJsonDecodingSpec DiscordAccessToken where
  type DecodingSpec DiscordAccessToken = JsonString
  fromJSONStructure = pure . DiscordAccessToken


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
  deriving ToJSON via (SpecJSON ProposalId)
instance HasJsonEncodingSpec ProposalId where
  type EncodingSpec ProposalId = JsonString
  toJSONStructure = UUID.toText . unProposalId


data NewProposalReq = NewProposalReq
  {         name :: Name
  , availability :: Availability
  ,  description :: Text
  ,        venue :: Text
  }
  deriving (FromJSON) via (SpecJSON NewProposalReq)
instance HasJsonDecodingSpec NewProposalReq where
  type DecodingSpec NewProposalReq =
    JsonObject
      '[ '("name", DecodingSpec Name)
       , '("availability", DecodingSpec Availability)
       , '("description", JsonString)
       , '("venue", JsonString)
       ]
  fromJSONStructure
      (Field @"name" rawName,
      (Field @"availability" rawAvailability,
      (Field @"description" description,
      (Field @"venue" venue,
      ()))))
    = do
      name <- fromJSONStructure rawName
      availability <- fromJSONStructure rawAvailability
      pure
        NewProposalReq
          { name
          , availability
          , description
          , venue
          }


newtype Availability = Availability
  { unAvailability :: Set Interval
  }
  deriving FromJSON via (SpecJSON Availability)
instance HasJsonDecodingSpec Availability where
  type DecodingSpec Availability = JsonArray (DecodingSpec Interval)
  fromJSONStructure =
      fmap (Availability . Set.fromList)
      . traverse fromJSONStructure


newtype Name = Name
  { unName :: Text
  }
  deriving (ToJSON, FromJSON) via (SpecJSON Name)
instance HasJsonEncodingSpec Name where
  type EncodingSpec Name = JsonString
  toJSONStructure = unName
instance HasJsonDecodingSpec Name where
  type DecodingSpec Name = EncodingSpec Name
  fromJSONStructure = pure . Name


data Interval = Interval
  { startInclusive :: UTCTime
  ,   endExclusive :: UTCTime
  }
  deriving stock (Eq, Ord)
  deriving (ToJSON, FromJSON) via (SpecJSON Interval)
instance HasJsonEncodingSpec Interval where
  type EncodingSpec Interval =
    JsonObject '[
      '("startInclusive", JsonDateTime),
      '("endExclusive", JsonDateTime)
    ]
  toJSONStructure
      Interval
        { startInclusive
        , endExclusive
        }
    =
      (Field @"startInclusive" startInclusive,
      (Field @"endExclusive" endExclusive,
      ()))
instance HasJsonDecodingSpec Interval where
  type DecodingSpec Interval = EncodingSpec Interval
  fromJSONStructure
      (Field @"startInclusive" startInclusive,
      (Field @"endExclusive" endExclusive,
      ()))
    = do
      pure
        Interval
          { startInclusive
          , endExclusive
          }


newtype DiscordUser = DiscordUser
  { unDiscordUser :: Text
  }
  deriving newtype ( Eq , FromJSON , Ord, Binary)
  deriving ToJSON via (SpecJSON DiscordUser)
instance HasJsonEncodingSpec DiscordUser where
  type EncodingSpec DiscordUser = JsonString
  toJSONStructure = unDiscordUser
instance HasJsonDecodingSpec DiscordUser where
  type DecodingSpec DiscordUser = EncodingSpec DiscordUser
  fromJSONStructure = pure . DiscordUser


data KV k v = KV
  {   key :: k
  , value :: v
  }
deriving via (SpecJSON (KV ProposalId Proposal)) instance
  ToJSON (KV ProposalId Proposal)
instance
    (HasJsonEncodingSpec k, HasJsonEncodingSpec v)
  =>
    HasJsonEncodingSpec (KV k v)
  where
    type EncodingSpec (KV k v) =
      JsonObject
        '[ '("key", EncodingSpec k)
         , '("value", EncodingSpec v)
         ]

    toJSONStructure KV { key , value } =
      (Field @"key" (toJSONStructure key),
      (Field @"value" (toJSONStructure value),
      ()))


