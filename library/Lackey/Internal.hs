{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
    This module is for internal use only and should be considered private. If
    you need anything exported from this module, please contact the maintainer.
-}
module Lackey.Internal where

import Data.Function ((&))
import Servant.API ((:>))

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Proxy as Proxy
import qualified GHC.TypeLits as GHC
import qualified Servant.API as Servant

data Method
    = DELETE
    | GET
    | POST
    deriving (Eq, Ord, Read, Show)

data Endpoint = Endpoint
    { endpointMethod :: Method
    , endpointPath :: [String]
    } deriving (Eq, Ord, Read, Show)

defaultEndpoint :: Endpoint
defaultEndpoint = Endpoint
    { endpointMethod = GET
    , endpointPath = []
    }

class HasRuby a where
    type Ruby a

    rubyFor :: Proxy.Proxy a -> Endpoint -> Ruby a

instance HasRuby (Servant.Delete a b) where
    type Ruby (Servant.Delete a b) = Endpoint

    rubyFor _proxy endpoint = endpoint
        { endpointMethod = DELETE
        }

instance HasRuby (Servant.Get a b) where
    type Ruby (Servant.Get a b) = Endpoint

    rubyFor _proxy endpoint = endpoint
        { endpointMethod = GET
        }

instance HasRuby (Servant.Post a b) where
    type Ruby (Servant.Post a b) = Endpoint

    rubyFor _proxy endpoint = endpoint
        { endpointMethod = POST
        }

instance (GHC.KnownSymbol a, HasRuby b) => HasRuby (a :> b) where
    type Ruby (a :> b) = Ruby b

    rubyFor _proxy endpoint = rubyFor (Proxy.Proxy :: Proxy.Proxy b) endpoint
        { endpointPath = endpointPath endpoint ++ [GHC.symbolVal (Proxy.Proxy :: Proxy.Proxy a)]
        }

methodName :: Endpoint -> String
methodName endpoint =
    let method = renderMethod endpoint
        pathSegments = case endpointPath endpoint of
            [] -> ["index"]
            x -> x
        path = List.intercalate "_" pathSegments
    in  method ++ "_" ++ path

renderMethod :: Endpoint -> String
renderMethod endpoint = endpoint & endpointMethod & show & map Char.toLower

renderPath :: Endpoint -> String
renderPath endpoint = case endpointPath endpoint of
    [] -> "/"
    path -> concatMap ('/' :) path

class HasCode a where
    codeFor :: a -> String

instance HasCode Endpoint where
    codeFor endpoint = case endpointMethod endpoint of
        DELETE -> "\
            \# @param http [Net::HTTP]\n\
            \# @return [Net::HTTPResponse]\n\
            \def " ++ methodName endpoint ++ "(http)\n\
            \  http." ++ renderMethod endpoint ++ "('" ++ renderPath endpoint ++ "')\n\
            \end\
        \"
        GET -> "\
            \# @param http [Net::HTTP]\n\
            \# @return [Net::HTTPResponse]\n\
            \def " ++ methodName endpoint ++ "(http)\n\
            \  http." ++ renderMethod endpoint ++ "('" ++ renderPath endpoint ++ "')\n\
            \end\
        \"
        POST -> "\
            \# @param http [Net::HTTP]\n\
            \# @return [Net::HTTPResponse]\n\
            \def " ++ methodName endpoint ++ "(http)\n\
            \  http." ++ renderMethod endpoint ++ "('" ++ renderPath endpoint ++ "', nil)\n\
            \end\
        \"

ruby :: (HasRuby a) => Proxy.Proxy a -> Ruby a
ruby proxy = rubyFor proxy defaultEndpoint

{- |
    Generate Ruby code from an API description.
-}
rubyForAPI :: (HasRuby a, HasCode (Ruby a)) => Proxy.Proxy a -> String
rubyForAPI proxy = codeFor (ruby proxy)
