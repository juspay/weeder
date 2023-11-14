{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language LambdaCase #-}

module Weeder.Config 
  ( Config(..)
  , defaultConfig
  , config
  ) 
   where

-- base
import Control.Applicative ((<|>))
import Data.Char (toLower)
import Data.List (intersperse, intercalate)

-- containers
import Data.Set ( Set )
import qualified Data.Set as Set

-- text
import qualified Data.Text as T
import Data.Text (Text)

-- toml-reader
import qualified Dhall


-- | Configuration for Weeder analysis.
data Config = Config
  { rootPatterns :: Set String
    -- ^ Any declarations matching these regular expressions will be added to
    -- the root set.
  , typeClassRoots :: Bool
    -- ^ If True, consider all declarations in a type class as part of the root
    -- set. Weeder is currently unable to identify whether or not a type class
    -- instance is used - enabling this option can prevent false positives.
  , unusedTypes :: Bool
  } deriving (Eq, Show)


defaultConfig :: Config
defaultConfig = Config
  { rootPatterns = Set.fromList [ "Main.main", "^Paths_.*"]
  , typeClassRoots = False
  , unusedTypes = False
  }


-- | A Dhall expression decoder for 'Config'.
--
-- This parses Dhall expressions of the type @{ roots : List Text, type-class-roots : Bool }@.
config :: Dhall.Decoder Config
config =
  Dhall.record do
    rootPatterns <- Set.fromList <$> Dhall.field "roots" ( Dhall.list Dhall.string )
    typeClassRoots <- Dhall.field "type-class-roots" Dhall.bool
    unusedTypes <- Dhall.field "unused-types" Dhall.bool

    return Config{..}
