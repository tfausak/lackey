module Lackey.Internal.Header where

newtype Header = Header String
    deriving (Eq, Ord, Read, Show)
