module Util
  ( Wrapper
  , unwrap
  , WMaybe
  , fromWMaybe
  , wmaybe
  , Rec
  , rec
  , unrec
  , decodeRecord
  , class GRecDecodeJson
  , gRecDecodeJson
  ) where

import Prelude
import CSS (a)
import Control.Plus (class Alt, class Plus, alt, empty)
import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (class MonoidRecord)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign.Object as FO
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Data.RowList (RLProxy(..))

newtype Wrapper a b
  = Wrapper (a b)

unwrap :: ∀ a b. Wrapper a b -> (a b)
unwrap (Wrapper a) = a

wrap :: ∀ a b. (a b) -> Wrapper a b
wrap = Wrapper

instance wrapperMonoid :: Plus p => Monoid (Wrapper p b) where
  mempty = wrap empty

instance wrapperSemigroup :: Alt a => Semigroup (Wrapper a b) where
  append a b = wrap $ alt (unwrap a) (unwrap b)

instance wrapperAlt :: Alt a => Alt (Wrapper a) where
  alt a b = a <> b

instance wrapperPlus :: Plus p => Plus (Wrapper p) where
  empty = mempty

instance wrapperFunctor :: Functor a => Functor (Wrapper a) where
  map f = wrap <<< map f <<< unwrap

instance wrapperApply :: Apply a => Apply (Wrapper a) where
  apply f a = wrap $ apply (unwrap f) (unwrap a)

instance wrapperApplicative :: Applicative a => Applicative (Wrapper a) where
  pure = wrap <<< pure

instance wrapperShow :: Show (a b) => Show (Wrapper a b) where
  show = unwrap >>> show

instance wrapperDecodeJson :: DecodeJson (a b) => DecodeJson (Wrapper a b) where
  decodeJson s = wrap <$> decodeJson s

instance wrapperEncodeJson :: EncodeJson (a b) => EncodeJson (Wrapper a b) where
  encodeJson = unwrap >>> encodeJson

type WMaybe
  = Wrapper Maybe

fromWMaybe :: ∀ a. a -> WMaybe a -> a
fromWMaybe a = unwrap >>> fromMaybe a

wmaybe :: ∀ a b. b -> (a -> b) -> WMaybe a -> b
wmaybe b f = unwrap >>> maybe b f

newtype Rec a
  = Rec (Record a)

rec :: ∀ a. Record a -> Rec a
rec = Rec

unrec :: ∀ a. Rec a -> Record a
unrec (Rec record) = record

instance recSemigroup :: Semigroup (Record a) => Semigroup (Rec a) where
  append a b = rec (unrec a <> unrec b)

instance recMonoid :: (RL.RowToList row list, MonoidRecord list row row) => Monoid (Rec row) where
  mempty = rec mempty

decodeRecord :: ∀ a. DecodeJson (Rec a) => Json -> Either String (Record a)
decodeRecord j = unrec <$> decodeJson j

instance decodeRec ::
  ( GRecDecodeJson row list
  , RL.RowToList row list
  ) =>
  DecodeJson (Rec row) where
  decodeJson json = case toObject json of
    Just object -> Rec <$> gRecDecodeJson object (RLProxy :: RLProxy list)
    Nothing -> Left "Could not convert JSON to object"

class GRecDecodeJson (row :: #Type) (list :: RL.RowList) | list -> row where
  gRecDecodeJson :: FO.Object Json -> RLProxy list -> Either String (Record row)

instance gDecodeJsonNil :: GRecDecodeJson () RL.Nil where
  gRecDecodeJson _ _ = Right {}

instance gDecodeJsonCons ::
  ( DecodeJson (a value)
  , Plus a
  , GRecDecodeJson rowTail tail
  , IsSymbol field
  , Row.Cons field (a value) rowTail row
  , Row.Lacks field rowTail
  ) =>
  GRecDecodeJson row (RL.Cons field (a value) tail) where
  gRecDecodeJson object _ = do
    let
      sProxy :: SProxy field
      sProxy = SProxy

      fieldName = reflectSymbol sProxy
    rest <- gRecDecodeJson object (RLProxy :: RLProxy tail)
    case FO.lookup fieldName object of
      Just jsonVal -> do
        val <- decodeJson jsonVal
        Right $ Record.insert sProxy val rest
      Nothing -> Right $ Record.insert sProxy empty rest
