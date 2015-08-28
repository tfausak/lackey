module Lackey.Internal.MatrixItem where

data MatrixItem
    = MatrixFlag String
    | MatrixParam String
    | MatrixParams String
    deriving (Eq, Ord, Read, Show)
