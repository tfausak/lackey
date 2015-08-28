module Lackey.Internal.Method where

data Method
    = Delete
    | Get
    | Patch
    | Post
    | Put
    deriving (Bounded, Enum, Eq, Ord, Read, Show)
