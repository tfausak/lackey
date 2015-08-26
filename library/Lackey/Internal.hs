{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{- |
    This module is for internal use only and should be considered private. If
    you need anything exported from this module, please contact the maintainer.
-}
module Lackey.Internal where

import qualified Data.Proxy as Proxy
import qualified Servant.API as Servant

data Endpoint = Endpoint
    { endpointMethod :: String
    } deriving (Eq, Ord, Read, Show)

defaultEndpoint :: Endpoint
defaultEndpoint = Endpoint
    { endpointMethod = "GET"
    }

class HasRuby a where
    type Ruby a

    rubyFor :: Proxy.Proxy a -> Endpoint -> Ruby a

instance HasRuby (Servant.Get a b) where
    type Ruby (Servant.Get a b) = Endpoint

    rubyFor _proxy endpoint = endpoint
        { endpointMethod = "GET"
        }

class HasCode a where
    codeFor :: a -> String

instance HasCode Endpoint where
    codeFor _endpoint = "\
        \# @param http [Net::HTTP]\n\
        \# @return [Net::HTTPResponse]\n\
        \def get_index(http)\n\
        \  http.get('/')\n\
        \end\
    \"

ruby :: (HasRuby a) => Proxy.Proxy a -> Ruby a
ruby proxy = rubyFor proxy defaultEndpoint

{- |
    Generate Ruby code from an API description.
-}
rubyForAPI :: (HasRuby a, HasCode (Ruby a)) => Proxy.Proxy a -> String
rubyForAPI proxy = codeFor (ruby proxy)
