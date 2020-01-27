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
  , prettifyJson
  , logs
  , logj
  , errors
  , errorj
  , mwhen
  , logo
  , css
  , Id
  , prettyShow
  , fromString
  , tryFromString
  , onEnter
  , setLocalStorage
  , getLocalStorage
  , id
  , diff
  , matchMaybe
  , dropUntil
  , inc
  , dec
  , Size
  ) where

import Prelude
import CSS (a)
import Control.Plus (class Alt, class Plus, alt, empty)
import Data.Argonaut.Core (Json, stringify, toObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.BigInt (BigInt, abs, fromInt, fromNumber, toNumber, toString)
import Data.BigInt as BigInt
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldr)
import Data.HeytingAlgebra (tt)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (class MonoidRecord)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (error, log)
import Foreign.Object as FO
import Halogen as H
import Halogen.HTML.Core as HC
import Halogen.HTML.Events (onKeyDown)
import Halogen.HTML.Properties (IProp(..))
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (imapState)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Routing.Match (Match(..), str)
import Type.Data.RowList (RLProxy(..))
import Web.UIEvent.KeyboardEvent (KeyboardEvent, code)

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

foreign import prettifyJson :: String -> String

foreign import logoImpl :: ∀ o. o -> Effect Unit

logo :: ∀ state a slot m o. o -> H.HalogenM state a slot m Aff Unit
logo = H.liftEffect <<< logoImpl

logs :: ∀ state a slot m s. Show s => s -> H.HalogenM state a slot m Aff Unit
logs = H.liftEffect <<< log <<< show

logj :: ∀ o state a slot m. EncodeJson o => o -> H.HalogenM state a slot m Aff Unit
logj = H.liftEffect <<< log <<< prettifyJson <<< stringify <<< encodeJson

errors :: ∀ state a slot m. String -> H.HalogenM state a slot m Aff Unit
errors = H.liftEffect <<< error

errorj :: ∀ o state a slot m. EncodeJson o => o -> H.HalogenM state a slot m Aff Unit
errorj = H.liftEffect <<< error <<< prettifyJson <<< stringify <<< encodeJson

mwhen :: ∀ m h. Monoid m => HeytingAlgebra h => Eq h => h -> m -> m
mwhen cond elems = if cond == tt then elems else mempty

css = HP.attr (HC.AttrName "style")

newtype Size
  = Size BigInt

derive instance sizeOrd :: Ord Size

derive instance sizeEq :: Eq Size

instance sizeDecodeJson :: DecodeJson Size where
  decodeJson json = do
    n <- decodeJson json
    maybe (Left $ "Can't convert " <> show n <> " to bigint") (Right <<< Size) $ fromNumber n

instance sizeEncodeJson :: EncodeJson Size where
  encodeJson (Size bigint) = encodeJson $ toNumber bigint

instance sizeShow :: Show Size where
  show (Size bigint) = toString bigint

newtype Id
  = Id BigInt

instance idDecodeJson :: DecodeJson Id where
  decodeJson json = do
    n <- decodeJson json
    maybe (Left $ "Can't convert " <> show n <> " to bigint") (Right <<< Id) $ fromNumber n

instance idEncodeJson :: EncodeJson Id where
  encodeJson (Id bigint) = encodeJson $ toNumber bigint

instance idShow :: Show Id where
  show (Id bigint) = toString bigint

derive instance idEq :: Eq Id

derive instance ordEq :: Ord Id

inc :: Id -> Id
inc (Id b) = Id $ b + fromInt 1

dec :: Id -> Id
dec (Id b) = Id $ b - fromInt 1

fromString :: String -> Id
fromString = fromMaybe (Id $ BigInt.fromInt (-1)) <<< tryFromString

tryFromString :: String -> Maybe Id
tryFromString "" = Nothing

tryFromString s = Id <$> BigInt.fromString s

diff :: Id -> Id -> BigInt
diff (Id b1) (Id b2) = abs $ b1 - b2

prettyShow :: Number -> String
prettyShow n = maybe (show n) toString $ fromNumber n

onEnter :: ∀ r i. i -> (KeyboardEvent -> i) -> IProp ( onKeyDown :: KeyboardEvent | r ) i
onEnter v v2 = onKeyDown $ \event -> if code event == "Enter" then Just v else Just $ v2 event

foreign import setLocalStorage :: String -> String -> Effect Unit

foreign import getLocalStorageImpl :: String -> (String -> Maybe String) -> Maybe String -> Effect (Maybe String)

getLocalStorage :: String -> Effect (Maybe String)
getLocalStorage s = getLocalStorageImpl s Just Nothing

id :: Match Id
id = fromString <$> str

matchMaybe :: ∀ a. Match a -> Match (Maybe a)
matchMaybe = map Just

dropUntil :: ∀ h a. HeytingAlgebra h => Eq h => (a -> h) -> Array a -> Array a
dropUntil f =
  _.result
    <<< foldr
        ( \a b ->
            if b.done then
              { done: true, result: b.result <> [ a ] }
            else
              b { done = f a == tt }
        )
        { done: false, result: [] }
