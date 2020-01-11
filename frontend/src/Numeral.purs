module Numeral (format, formatBytes, formatNumber) where

foreign import formatImpl :: Number -> String -> String

format :: String -> Number -> String
format s i = formatImpl i s

formatBytes :: Number -> String
formatBytes = format "0.00b"

formatNumber :: Number -> String
formatNumber = format "0.00a"
