module Lackey.Internal.Endpoint where

import Lackey.Internal.Header (Header)
import Lackey.Internal.Method (Method (Get))
import Lackey.Internal.PathSegment (PathSegment)
import Lackey.Internal.QueryItem (QueryItem)

data Endpoint = Endpoint
    { endpointMethod :: Method
    , endpointPathSegments :: [PathSegment]
    , endpointQueryItems :: [QueryItem]
    , endpointHeaders :: [Header]
    , endpointHasBody :: Bool
    } deriving (Eq, Ord, Read, Show)

defaultEndpoint :: Endpoint
defaultEndpoint = Endpoint
    { endpointMethod = Get
    , endpointPathSegments = []
    , endpointQueryItems = []
    , endpointHeaders = []
    , endpointHasBody = False
    }
