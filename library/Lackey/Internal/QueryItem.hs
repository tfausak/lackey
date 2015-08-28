module Lackey.Internal.QueryItem where

data QueryItem
    = QueryFlag String
    | QueryParam String
    | QueryParams String
    deriving (Eq, Ord, Read, Show)
