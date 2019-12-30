module Util
  ( Wrapper
  , unwrap
  , WMaybe
  , fromWMaybe
  , wmaybe
  ) where

import Prelude
import Control.Plus (class Alt, class Plus, alt, empty)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Maybe (Maybe, fromMaybe, maybe)

newtype Wrapper a
  = Wrapper a

unwrap :: ∀ a. Wrapper a -> a
unwrap (Wrapper a) = a

instance wrapperMonoid :: Plus p => Monoid (Wrapper (p b)) where
  mempty = Wrapper empty

instance wrapperSemigroup :: Alt a => Semigroup (Wrapper (a b)) where
  append (Wrapper a) (Wrapper b) = Wrapper $ alt a b

instance wrapperShow :: Show a => Show (Wrapper a) where
  show = unwrap >>> show

instance wrapperDecodeJson :: DecodeJson a => DecodeJson (Wrapper a) where
  decodeJson s = decodeJson s <#> Wrapper

instance wrapperEncodeJson :: EncodeJson a => EncodeJson (Wrapper a) where
  encodeJson = unwrap >>> encodeJson

type WMaybe w
  = Wrapper (Maybe w)

fromWMaybe :: ∀ a. a -> WMaybe a -> a
fromWMaybe a = unwrap >>> fromMaybe a

wmaybe :: ∀ a b. b -> (a -> b) -> WMaybe a -> b
wmaybe b f = unwrap >>> maybe b f
