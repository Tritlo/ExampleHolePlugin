module Test.ProgInput where

data ProgInput = ProgIn { modN :: String,
                          propN :: String,
                          holeN :: Maybe String,
                          holeL :: Maybe String,
                          fitStrs :: [String] } deriving (Show, Read)

