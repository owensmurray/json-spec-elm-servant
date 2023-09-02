{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Api (Api)
import Data.Foldable (traverse_)
import Data.HashMap.Strict (HashMap)
import Data.JsonSpec.Elm.Servant (servantDefs)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Language.Elm.Name (Module)
import Language.Elm.Pretty (modules)
import Prelude (Bool(True), Functor(fmap), Semigroup((<>)), ($), (.),
  FilePath, IO, init)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Servant.API (ToServantApi)
import System.Directory (createDirectoryIfMissing)
import System.Process (callCommand)
import Test.Hspec (describe, hspec, it)
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO

main :: IO ()
main =
  hspec $ do
    describe "thing" $ do
      it "works" $
        let
          actual :: HashMap Module Text
          actual =
            fmap ((<> "\n") . renderStrict . layoutPretty defaultLayoutOptions)
            . modules
            . Set.toList
            $ servantDefs (Proxy @(ToServantApi Api))

        in do
          traverse_ writeModule (HM.toList actual)
          callCommand "(cd elm-test; elm-format src/ --yes)"
          callCommand
            "(\
              \cd elm-test; \
              \yes Y | (\
                \elm init; \
                \elm install rtfeldman/elm-iso8601-date-strings; \
                \elm install elm/json; \
                \elm install elm/url; \
                \elm install elm/time; \
                \elm install elm/http\
              \); \
              \elm make src/Api/Req.elm\
            \)"
          callCommand "rm -rf elm-test"


writeModule :: (Module, Text) -> IO ()
writeModule (module_, content) = do
    createDirectoryIfMissing True dirname
    TIO.writeFile filename content
  where
    pathName :: [Text] -> FilePath
    pathName = ("elm-test/src/" <>) . Text.unpack . Text.intercalate "/"

    filename :: FilePath
    filename = pathName module_ <> ".elm"

    dirname :: FilePath
    dirname = pathName (init module_)


