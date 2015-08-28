module Lackey.Internal.PathSegment where

import Lackey.Internal.MatrixItem (MatrixItem)

data PathSegment
    = PathLiteral String
    | PathCapture String
    | PathMatrix MatrixItem
    deriving (Eq, Ord, Read, Show)

isPathMatrix :: PathSegment -> Bool
isPathMatrix (PathMatrix _) = True
isPathMatrix _ = False
