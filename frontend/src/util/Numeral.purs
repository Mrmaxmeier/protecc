module Numeral (format, formatBytes, formatNumber, formatPercent, formatSize, formatMillis, formatBytesNumber) where

import Prelude
import Util (Size, toNumber)

foreign import formatImpl :: Number -> String -> String

format :: String -> Number -> String
format s i = formatImpl i s

formatBytes :: Size -> String
formatBytes = formatBytesNumber <<< toNumber

formatBytesNumber :: Number -> String
formatBytesNumber = format "0.00b"

formatNumber :: Number -> String
formatNumber = format "0.00a"

formatPercent :: Number -> String
formatPercent = format "0.00"

formatSize :: Size -> String
formatSize = formatNumber <<< toNumber

formatMillis :: Number -> String
formatMillis = format "00:00:00" <<< (_ / 1000.0)
