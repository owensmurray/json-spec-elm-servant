{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Data.JsonSpec.Elm.Servant (
  servantDefs,
) where


import Bound (Var(B, F), Scope, abstract1, closed, toScope)
import Control.Monad.Writer (MonadTrans(lift), MonadWriter(tell),
  execWriter)
import Data.Foldable (Foldable(fold))
import Data.JsonSpec (HasJsonDecodingSpec(DecodingSpec),
  HasJsonEncodingSpec(EncodingSpec))
import Data.JsonSpec.Elm (HasType(decoderOf, encoderOf, typeOf),
  Definitions)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.Set (Set)
import Data.String (IsString(fromString))
import Data.Text (Text)
import Data.Void (Void, absurd)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Language.Elm.Definition (Definition)
import Language.Elm.Expression ((<|), Expression)
import Language.Elm.Type (Type)
import Network.HTTP.Types (Method)
import Prelude (Applicative(pure), Foldable(foldr, length), Maybe(Just,
  Nothing), Semigroup((<>)), ($), (.), (<$>), Eq, Int, error, reverse)
import Servant.API (ReflectMethod(reflectMethod), (:<|>), (:>), Capture,
  Header', Headers, JSON, NamedRoutes, NoContent, NoContentVerb, Optional,
  ReqBody', Required, ToServantApi, Verb)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Language.Elm.Definition as Def
import qualified Language.Elm.Expression as Expr
import qualified Language.Elm.Name as Name
import qualified Language.Elm.Pattern as Pat
import qualified Language.Elm.Type as Type


servantDefs :: forall api. (Elmable api) => Proxy api -> Set Definition
servantDefs _ =
  builtins
  <> execWriter (endpoints @api [])


builtins :: Set Definition
builtins =
  Set.fromList
    [ Def.Alias
        "Api.Req.Request"
        1
        (
          toScope $
            Type.Record
              [ (Name.Field "method", "Basics.String")
              , (Name.Field "headers", "Basics.List" `Type.App` "Http.Header")
              , (Name.Field "url", "Basics.String")
              , (Name.Field "body", "Http.Body")
              , ( Name.Field "decoder"
                , "Api.Req.Either"
                    `Type.App` Type.Var (B 0)
                    `Type.App` ("Json.Decode.Decoder" `Type.App` Type.Var (B 0))
                )
              ]
        )
    , Def.Type
        "Api.Req.Either"
        2
        [ ( Name.Constructor "Left"
          , [toScope (Type.Var (B 0))]
          )
        , ( Name.Constructor "Right"
          , [toScope (Type.Var (B 1))]
          )
        ]
    , Def.Constant
        "Api.Req.task"
        1
        (
          toScope $
            let
              var :: Type (Bound.Var Int a)
              var = Type.Var (B 0)
            in
              Type.Fun
                ("Api.Req.Request" `Type.App` var)
                (Type.apps "Task.Task" ["Http.Error", var])
        )
        (
          Expr.Lam . toScope $
            let
              req :: Expression (Bound.Var () a)
              req = Expr.Var (B ())

              f :: Text -> b -> (Name.Field, b)
              f name expr = (Name.Field name, expr)

              p :: Expression v -> Text -> Expression v
              p v name = Expr.Proj (Name.Field name) `Expr.App` v
            in
              "Http.task" <|
                Expr.Record
                  [ f "method"   $ p req "method"
                  , f "headers"  $ p req "headers"
                  , f "url"      $ p req "url"
                  , f "body"     $ p req "body"
                  , f "timeout"    "Maybe.Nothing"
                  , f "resolver" $
                      "Http.stringResolver" `Expr.App`
                        (
                          Expr.Lam . toScope $
                            let
                              var :: Expression (Bound.Var () a)
                              var = Expr.Var (B ())

                              pat
                                :: Name.Qualified
                                -> [Pat.Pattern v]
                                -> Expression (Bound.Var b a)
                                -> (Pat.Pattern v, Scope b Expression a)
                              pat con vars expr =
                                (Pat.Con con vars, toScope expr)

                              patVar :: Int -> Expression (Bound.Var Int a)
                              patVar n = Expr.Var (B n)
                            in
                              Expr.Case
                                var
                                [ pat "Http.BadUrl_" [Pat.Var 0] $
                                    "Result.Err" `Expr.App`
                                      ("Http.BadUrl" `Expr.App` patVar 0)
                                , pat "Http.Timeout_" [] $
                                    "Result.Err" `Expr.App` "Http.Timeout"
                                , pat "Http.NetworkError_" [] $
                                    "Result.Err" `Expr.App` "Http.NetworkError"
                                , pat "Http.BadStatus_" [Pat.Var 0, Pat.Var 1] $
                                    "Result.Err" `Expr.App`
                                      (
                                        "Http.BadStatus" `Expr.App`
                                          p (patVar 0) "statusCode"
                                      )
                                , pat
                                    "Http.GoodStatus_"
                                    [Pat.Var 0, Pat.Var 1]
                                    (
                                      Expr.Case
                                        ( F . F <$> p req "decoder")
                                        [ pat "Api.Req.Left" [Pat.Var 0] $
                                            "Result.Ok" `Expr.App` patVar 0
                                        , pat "Api.Req.Right" [Pat.Var 0] $
                                            Expr.Case
                                              (
                                                Expr.apps
                                                  "Json.Decode.decodeString"
                                                  [ patVar 0
                                                  , F <$> patVar 1
                                                  ]
                                              )
                                              [ pat "Result.Err" [Pat.Var 0] $
                                                  "Result.Err"
                                                    <| "Http.BadBody"
                                                    <| "Json.Decode.errorToString"
                                                    <| patVar 0
                                              , pat "Result.Ok" [Pat.Var 0] $
                                                  "Result.Ok" <| patVar 0
                                              ]
                                        ]
                                    )
                                ]
                        )
                  ]

        )
    ]


{-| Class of servant APIs for which Elm client code can be generated. -}
class Elmable e where
  {-|
    Collect all the Elm definitions needed to implement a client for
    the API.  This is called recursively on our walk down the API tree,
    and the @['Param']@ argument contains all the request parameters
    (like 'Capture', 'ReqBody', etc) that have been encountered so far on
    whatever particular branch . It will start out empty at the API root.
  -}
  endpoints :: [Param] -> Definitions ()
instance (Elmable a, Elmable b) => Elmable (a :<|> b) where
  endpoints params = do
    endpoints @a params
    endpoints @b params
instance (Elmable (ToServantApi api)) => Elmable (NamedRoutes api) where
  endpoints = endpoints @(ToServantApi api)
instance (IsParam a, Elmable b) => Elmable (a :> b) where
  endpoints params = do
    p <- param @a
    endpoints @b (p : params)
instance (Elmable (Verb m c t r)) => Elmable (Verb m c t (Headers h r)) where
  endpoints = endpoints @(Verb m c t r)
instance {- Elmable (Verb m c t NoContent) -}
    (Elmable (NoContentVerb m))
  =>
    Elmable (Verb m c t NoContent)
  where
    endpoints = endpoints @(NoContentVerb m)
instance {- Elmable (Verb method code types response) -}
    {-# overlaps #-}
    ( HasType (EncodingSpec response)
    , ReflectMethod method
    )
  =>
    Elmable (Verb method code types response)
  where
    endpoints (reverse -> params) = do
      responseType <- typeOf @(EncodingSpec response)
      decoder <- decoderOf @(EncodingSpec response)
      tell . Set.singleton $
        Def.Constant
          (requestFunctionName @method params)
          (length params)
          (requestFunctionType params responseType)
          (
            requestFunctionBody
              params
              (reflectMethod (Proxy @method))
              ("Api.Req.Right" `Expr.App` decoder)
          )
      pure ()
instance (ReflectMethod method) => Elmable (NoContentVerb method) where
  endpoints (reverse -> params) = do
    tell . Set.singleton $
      Def.Constant
        (requestFunctionName @method params)
        (length params)
        (requestFunctionType params "Basics.()")
        (
          requestFunctionBody
            params
            (reflectMethod (Proxy @method))
            ("Api.Req.Left" `Expr.App` "Basics.()")
        )
    pure ()


{-|
  Obtain a value-level request parameter type from the type-level servant
  parameter type.
-}
class IsParam a where
  param :: Definitions Param
instance (KnownSymbol name) => IsParam (Capture name tpy) where
  param = pure $ PathParam (Capture (sym @name))
instance (KnownSymbol name) => IsParam (Header' (Optional : mods) name a) where
  param = pure $ HeaderParam (OptionalHeader (sym @name))
instance (KnownSymbol name) => IsParam (Header' (Required : mods) name a) where
  param = pure $ HeaderParam (RequiredHeader (sym @name))
instance {- IsParam (Header' (other : mods) name a) -}
    {-# OVERLAPS #-} (IsParam (Header' mods name a))
  =>
    IsParam (Header' (other : mods) name a)
  where
    param = param @(Header' mods name a)
instance {- IsParam (ReqBody' (Required : mods) (JSON : accept) a) -}
    (HasType (DecodingSpec a))
  =>
    IsParam (ReqBody' (Required : mods) (JSON : accept) a)
  where
    param = do
      elmType <- typeOf @(DecodingSpec a)
      encoder <- encoderOf @(DecodingSpec a)
      pure $ BodyEncoder {elmType, encoder}
instance {- IsParam (ReqBody' (other : mods) (JSON : accept) a) -}
    {-# overlaps #-} (IsParam (ReqBody' mods '[JSON] a))
  =>
    IsParam (ReqBody' (other : mods) (JSON : accept) a)
  where
    param = param @(ReqBody' mods '[JSON] a)
instance {- IsParam (ReqBody' mods (other : accept) a) -}
    {-# overlaps #-} (IsParam (ReqBody' mods accept a))
  =>
    IsParam (ReqBody' mods (other : accept) a)
  where
    param = param @(ReqBody' mods accept a)
instance (KnownSymbol segment) => IsParam (segment :: Symbol) where
  param = pure $ PathParam (Static (sym @segment))


requestFunctionName
  :: forall method. (ReflectMethod method)
  => [Param]
  -> Name.Qualified
requestFunctionName params =
    Name.Qualified
      ["Api", "Req"]
      (fold (methodName : pathParts))
  where
    methodName :: Text
    methodName =
      Text.toLower
      . TE.decodeUtf8
      . reflectMethod
      $ Proxy @method

    pathParts :: [Text]
    pathParts =
      Text.toTitle <$>
        mapMaybe
          (\case
            PathParam (Static segment) -> Just segment
            PathParam (Capture name) -> Just name
            _ -> Nothing
          )
          params


requestFunctionType
  :: [Param]
  -> Type Void
  -> Scope Int Type Void
requestFunctionType params responseType =
    lift funType
  where
    funType :: Type Void
    funType =
      foldr
        Type.Fun
        ("Api.Req.Request" `Type.App` responseType)
        (
          mapMaybe
            (\case
              PathParam (Capture _) -> Just "Basics.String"
              PathParam (Static _) -> Nothing
              HeaderParam (RequiredHeader _) -> Just "Basics.String"
              HeaderParam (OptionalHeader _) ->
                Just ("Basics.Maybe" `Type.App` "Basics.String")
              BodyEncoder typ _ -> Just typ
            )
            params
        )


requestFunctionBody
  :: [Param]
  -> Method
  -> Expression Void
  -> Expression Void
requestFunctionBody params method decoder =
    buildLambda
      (reverse params)
      (
        Expr.Record
          [ (Name.Field "method", Expr.String (TE.decodeUtf8 method))
          , (Name.Field "headers", headers)
          , (Name.Field "url", url)
          , (Name.Field "body", body)
          , (Name.Field "decoder", Expr.bind g absurd decoder)
          ]
      )
  where
    headers :: Expression Param
    headers =
        Expr.apps
          "List.filterMap"
          [ "Basics.identity"
          , Expr.List
              [ headerExpr header
              | HeaderParam header <- params
              ]
          ]
      where
        headerExpr :: HeaderParam -> Expression Param
        headerExpr header =
            Expr.apps
              "Maybe.map"
              [ "Http.header" `Expr.App` Expr.String name
              , case header of
                  RequiredHeader _ ->
                    "Maybe.Just" `Expr.App` Expr.Var (HeaderParam header)
                  OptionalHeader _ -> Expr.Var (HeaderParam header)
              ]
          where
            name :: Text
            name =
              case header of
                  RequiredHeader n -> n
                  OptionalHeader n -> n

    url :: Expression Param
    url =
      Expr.apps
        "Url.Builder.absolute"
        [
          Expr.List
            [ case pp of
                Static part -> Expr.String part
                Capture _ -> Expr.Var param_
            | param_@(PathParam pp) <- params
            ]
        , Expr.List []
        ]

    body :: Expression Param
    body =
      case
        [ g "Http.jsonBody" `Expr.App`
            ((absurd <$> encoder) `Expr.App` Expr.Var param_)
        | param_@(BodyEncoder _ encoder) <- params
        ]
      of
        [] -> g "Http.emptyBody"
        (encoder : _) -> encoder

    buildLambda :: [Param] -> Expression Param -> Expression Void
    buildLambda = \cases
      [] e ->
        fromMaybe
          (error "Paramaters in expression to not match the parameter list.")
          (Bound.closed e)
      (PathParam (Static _) : more) e ->
        buildLambda more e
      (p : more) e ->
        buildLambda
          more
          (Expr.Lam (abstract1 p e))


data Param
  = PathParam PathParam
  | HeaderParam HeaderParam
  | BodyEncoder
      { elmType :: Type Void
      , encoder :: Expression Void
      }
  deriving stock (Eq)


data PathParam
  = Static Text
  | Capture Text
  deriving stock (Eq)


data HeaderParam
  = RequiredHeader Text
  | OptionalHeader Text
  deriving stock (Eq)


g :: Name.Qualified -> Expression any
g = Expr.Global


sym
  :: forall a b.
     ( IsString b
     , KnownSymbol a
     )
  => b
sym = fromString $ symbolVal (Proxy @a)


