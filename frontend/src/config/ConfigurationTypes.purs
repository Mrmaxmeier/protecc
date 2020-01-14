module ConfigurationTypes where

import Prelude
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, toUnfoldable)
import Util (Id, fromString)

type Tag
  = { id :: Id, slug :: String, name :: String, color :: String, owner :: String }

type TagMessage
  = { slug :: String, name :: String, color :: String, owner :: String }

type Service
  = { id :: Id, slug :: String, port :: Int, name :: String }

type ServiceMessage
  = { slug :: String, port :: Int, name :: String }

type Configuration
  = { tags :: Array Tag, services :: Array Service }

type ConfigurationMessage
  = { tags :: Object { slug :: String, name :: String, color :: String, owner :: String }, services :: Object { slug :: String, port :: Int, name :: String } }

fromMessage :: ConfigurationMessage -> Configuration
fromMessage msg =
  { tags: map (\(Tuple id tag) -> { id: fromString id, slug: tag.slug, name: tag.name, color: tag.color, owner: tag.owner }) $ toUnfoldable msg.tags
  , services: map (\(Tuple id service) -> { id: fromString id, slug: service.slug, name: service.name, port: service.port }) $ toUnfoldable msg.services
  }

toTagMessage :: Tag -> TagMessage
toTagMessage tag = { slug: tag.slug, name: tag.name, color: tag.color, owner: tag.owner }

toServiceMessage :: Service -> ServiceMessage
toServiceMessage service = { slug: service.slug, name: service.name, port: service.port }
