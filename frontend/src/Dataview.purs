module Dataview (render, DisplayType(..), displayTypeToString, displayTypeFromString) where

import Prelude
import CSS (Color, rgb)
import CSS as CSS
import Data.Either (Either(..))
import Data.Foldable (all)
import Data.String (CodePoint, Pattern(..), Replacement(..), codePointFromChar, replaceAll, toCodePointArray)
import Data.String.Base64 as Base64
import Effect.Exception (message)
import Halogen as H
import Halogen.HTML (HTML, text)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties (classes)
import SemanticUI as S

data DisplayType
  = Auto
  | Ascii
  | B64
  | Hexdump

derive instance eqDisplayType :: Eq DisplayType

displayTypeToString :: DisplayType -> String
displayTypeToString = case _ of
  Auto -> "auto"
  Ascii -> "ascii"
  B64 -> "b64"
  Hexdump -> "hexdump"

displayTypeFromString :: String -> DisplayType
displayTypeFromString = case _ of
  "auto" -> Auto
  "ascii" -> Ascii
  "b64" -> B64
  _ -> Hexdump

type HexdumpRow
  = { number :: String
    , hex :: String
    , ascii :: String
    }

foreign import hexdump :: Array CodePoint -> Array HexdumpRow

printable :: CodePoint -> Boolean
printable c = c == codePointFromChar '\r' || c == codePointFromChar '\n' || (c >= codePointFromChar ' ' && c < codePointFromChar '~')

decb64 :: ∀ w i. String -> (String -> HTML w i) -> HTML w i
decb64 s f = case Base64.decode s of
  Left _ -> HH.pre [ classes [ S.wrap ] ] [ text $ "Couldn't parse b64: " <> s ]
  Right s -> f s

render :: ∀ w i. DisplayType -> String -> HTML w i
render _ "" = HH.pre [ classes [ S.scroll ] ] []

render Auto s = decb64 s $ \decoded -> if all printable $ toCodePointArray decoded then render Ascii s else render Hexdump s

render B64 s = HH.pre [ classes [ S.wrap ] ] [ text s ]

render Ascii s = decb64 s $ \decoded -> HH.pre [ classes [ S.scroll ] ] [ text $ replaceAll (Pattern "\n") (Replacement "↵\n") decoded ]

render Hexdump s =
  decb64 s
    $ \decoded ->
        let
          rows = hexdump $ toCodePointArray decoded
        in
          HH.pre [ classes [ S.scroll ] ]
            $ map
                ( \row ->
                    HH.span_
                      [ HH.span [ HC.style $ CSS.color $ rgb 0xA0 0xA0 0x00 ] [ text row.number ]
                      , text ": "
                      , HH.span [ HC.style $ CSS.color $ rgb 0x66 0x66 0x66 ] [ text row.hex ]
                      , text "  "
                      , HH.span [ HC.style $ CSS.color $ rgb 0xBA 0x21 0x21 ] [ text row.ascii ]
                      , text "\n"
                      ]
                )
                rows
