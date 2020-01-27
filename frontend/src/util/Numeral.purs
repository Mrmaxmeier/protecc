module Numeral (format, formatBytes, formatNumber, formatPercent, formatSize) where

import Prelude
import Util (Size, toNumber)

foreign import formatImpl :: Number -> String -> String

format :: String -> Number -> String
format s i = formatImpl i s

formatBytes :: Size -> String
formatBytes = format "0.00b" <<< toNumber

formatNumber :: Number -> String
formatNumber = format "0.00a"

formatPercent :: Number -> String
formatPercent = format "0.00"

formatSize :: Size -> String
formatSize = formatNumber <<< toNumber
