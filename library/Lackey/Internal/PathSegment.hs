module Lackey.Internal.PathSegment where

data PathSegment
    = PathLiteral String
    | PathCapture String
    deriving (Eq, Ord, Read, Show)
