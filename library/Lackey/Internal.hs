{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
    This module is for internal use only and should be considered private. If
    you need anything exported from this module, please contact the maintainer.
-}
module Lackey.Internal where

import Flow
import Servant.API ((:>), (:<|>)(..))

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Proxy as Proxy
import qualified GHC.TypeLits as GHC
import qualified Servant.API as Servant

data Method
    = Delete
    | Get
    | Post
    deriving (Eq, Ord, Read, Show)

data PathSegment
    = PathLiteral String
    deriving (Eq, Ord, Read, Show)

data Endpoint = Endpoint
    { endpointMethod :: Method
    , endpointPathSegments :: [PathSegment]
    } deriving (Eq, Ord, Read, Show)

defaultEndpoint :: Endpoint
defaultEndpoint = Endpoint
    { endpointMethod = Get
    , endpointPathSegments = []
    }

class HasRuby a where
    type Ruby a

    rubyFor :: Proxy.Proxy a -> Endpoint -> Ruby a

instance HasRuby (Servant.Delete a b) where
    type Ruby (Servant.Delete a b) = Endpoint

    rubyFor _ endpoint = endpoint
        { endpointMethod = Delete
        }

instance HasRuby (Servant.Get a b) where
    type Ruby (Servant.Get a b) = Endpoint

    rubyFor _ endpoint = endpoint
        { endpointMethod = Get
        }

instance HasRuby (Servant.Post a b) where
    type Ruby (Servant.Post a b) = Endpoint

    rubyFor _ endpoint = endpoint
        { endpointMethod = Post
        }

instance (GHC.KnownSymbol a, HasRuby b) => HasRuby (a :> b) where
    type Ruby (a :> b) = Ruby b

    rubyFor _ endpoint = rubyFor (Proxy.Proxy :: Proxy.Proxy b) endpoint
        { endpointPathSegments = endpointPathSegments endpoint ++
            [ PathLiteral (GHC.symbolVal (Proxy.Proxy :: Proxy.Proxy a))
            ]
        }

instance (HasRuby a, HasRuby b) => HasRuby (a :<|> b) where
    type Ruby (a :<|> b) = Ruby a :<|> Ruby b

    rubyFor _ endpoint
        = rubyFor (Proxy.Proxy :: Proxy.Proxy a) endpoint
        :<|> rubyFor (Proxy.Proxy :: Proxy.Proxy b) endpoint

methodName :: Endpoint -> String
methodName endpoint =
    let method = renderMethod endpoint
        pathSegments = case endpointPathSegments endpoint of
            [] -> ["index"]
            segments -> flip map segments <| \ segment -> case segment of
                PathLiteral literal -> literal
        path = List.intercalate "_" pathSegments
    in  method ++ "_" ++ path

renderMethod :: Endpoint -> String
renderMethod endpoint = endpoint |> endpointMethod |> show |> map Char.toLower

renderPath :: Endpoint -> String
renderPath endpoint = case endpointPathSegments endpoint of
    [] -> "/"
    segments -> flip concatMap segments <| \ segment -> case segment of
        PathLiteral literal -> '/' : literal

class HasCode a where
    codeFor :: a -> String

instance HasCode Endpoint where
    codeFor endpoint = case endpointMethod endpoint of
        Delete -> "\
            \# @param http [Net::HTTP]\n\
            \# @return [Net::HTTPResponse]\n\
            \def " ++ methodName endpoint ++ "(http)\n\
            \  http." ++ renderMethod endpoint ++ "('" ++ renderPath endpoint ++ "')\n\
            \end\
        \"
        Get -> "\
            \# @param http [Net::HTTP]\n\
            \# @return [Net::HTTPResponse]\n\
            \def " ++ methodName endpoint ++ "(http)\n\
            \  http." ++ renderMethod endpoint ++ "('" ++ renderPath endpoint ++ "')\n\
            \end\
        \"
        Post -> "\
            \# @param http [Net::HTTP]\n\
            \# @return [Net::HTTPResponse]\n\
            \def " ++ methodName endpoint ++ "(http)\n\
            \  http." ++ renderMethod endpoint ++ "('" ++ renderPath endpoint ++ "', nil)\n\
            \end\
        \"

instance (HasCode a, HasCode b) => HasCode (a :<|> b) where
    codeFor (x :<|> y) = concat [codeFor x, "\n\n", codeFor y]

ruby :: (HasRuby a) => Proxy.Proxy a -> Ruby a
ruby proxy = rubyFor proxy defaultEndpoint

{- |
    Generate Ruby code from an API description.
-}
rubyForAPI :: (HasRuby a, HasCode (Ruby a)) => Proxy.Proxy a -> String
rubyForAPI proxy = codeFor (ruby proxy)
