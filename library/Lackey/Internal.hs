{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
    This module is for internal use only and should be considered private. If
    you need anything exported from this module, please contact the maintainer.
-}
module Lackey.Internal where

import Flow
import Lackey.Internal.MatrixItem
import Lackey.Internal.Method
import Lackey.Internal.PathSegment
import Lackey.Internal.QueryItem
import Servant.API ((:>), (:<|>)(..))

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Proxy as Proxy
import qualified GHC.TypeLits as GHC
import qualified Servant.API as Servant

data Endpoint = Endpoint
    { endpointMethod :: Method
    , endpointPathSegments :: [PathSegment]
    , endpointQueryItems :: [QueryItem]
    , endpointHasBody :: Bool
    } deriving (Eq, Ord, Read, Show)

defaultEndpoint :: Endpoint
defaultEndpoint = Endpoint
    { endpointMethod = Get
    , endpointPathSegments = []
    , endpointQueryItems = []
    , endpointHasBody = False
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

instance HasRuby (Servant.Patch a b) where
    type Ruby (Servant.Patch a b) = Endpoint

    rubyFor _ endpoint = endpoint
        { endpointMethod = Patch
        }

instance HasRuby (Servant.Post a b) where
    type Ruby (Servant.Post a b) = Endpoint

    rubyFor _ endpoint = endpoint
        { endpointMethod = Post
        }

instance HasRuby (Servant.Put a b) where
    type Ruby (Servant.Put a b) = Endpoint

    rubyFor _ endpoint = endpoint
        { endpointMethod = Put
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

instance (GHC.KnownSymbol a, HasRuby c) =>
        HasRuby (Servant.Capture a b :> c) where
    type Ruby (Servant.Capture a b :> c) = Ruby c

    rubyFor _ endpoint = rubyFor (Proxy.Proxy :: Proxy.Proxy c) endpoint
        { endpointPathSegments = endpointPathSegments endpoint ++
            [ PathCapture (GHC.symbolVal (Proxy.Proxy :: Proxy.Proxy a))
            ]
        }

instance (GHC.KnownSymbol a, HasRuby b) =>
        HasRuby (Servant.MatrixFlag a :> b) where
    type Ruby (Servant.MatrixFlag a :> b) = Ruby b

    rubyFor _ endpoint = rubyFor (Proxy.Proxy :: Proxy.Proxy b) endpoint
        { endpointPathSegments = endpointPathSegments endpoint ++
            [ PathMatrix (MatrixFlag (GHC.symbolVal (Proxy.Proxy :: Proxy.Proxy a)))
            ]
        }

instance (GHC.KnownSymbol a, HasRuby c) =>
        HasRuby (Servant.MatrixParam a b :> c) where
    type Ruby (Servant.MatrixParam a b :> c) = Ruby c

    rubyFor _ endpoint = rubyFor (Proxy.Proxy :: Proxy.Proxy c) endpoint
        { endpointPathSegments = endpointPathSegments endpoint ++
            [ PathMatrix (MatrixParam (GHC.symbolVal (Proxy.Proxy :: Proxy.Proxy a)))
            ]
        }

instance (GHC.KnownSymbol a, HasRuby c) =>
        HasRuby (Servant.MatrixParams a b :> c) where
    type Ruby (Servant.MatrixParams a b :> c) = Ruby c

    rubyFor _ endpoint = rubyFor (Proxy.Proxy :: Proxy.Proxy c) endpoint
        { endpointPathSegments = endpointPathSegments endpoint ++
            [ PathMatrix (MatrixParams (GHC.symbolVal (Proxy.Proxy :: Proxy.Proxy a)))
            ]
        }

instance (GHC.KnownSymbol a, HasRuby b) =>
        HasRuby (Servant.QueryFlag a :> b) where
    type Ruby (Servant.QueryFlag a :> b) = Ruby b

    rubyFor _ endpoint = rubyFor (Proxy.Proxy :: Proxy.Proxy b) endpoint
        { endpointQueryItems = endpointQueryItems endpoint ++
            [ QueryFlag (GHC.symbolVal (Proxy.Proxy :: Proxy.Proxy a))
            ]
        }

instance (GHC.KnownSymbol a, HasRuby c) =>
        HasRuby (Servant.QueryParam a b :> c) where
    type Ruby (Servant.QueryParam a b :> c) = Ruby c

    rubyFor _ endpoint = rubyFor (Proxy.Proxy :: Proxy.Proxy c) endpoint
        { endpointQueryItems = endpointQueryItems endpoint ++
            [ QueryParam (GHC.symbolVal (Proxy.Proxy :: Proxy.Proxy a))
            ]
        }

instance (GHC.KnownSymbol a, HasRuby c) =>
        HasRuby (Servant.QueryParams a b :> c) where
    type Ruby (Servant.QueryParams a b :> c) = Ruby c

    rubyFor _ endpoint = rubyFor (Proxy.Proxy :: Proxy.Proxy c) endpoint
        { endpointQueryItems = endpointQueryItems endpoint ++
            [ QueryParams (GHC.symbolVal (Proxy.Proxy :: Proxy.Proxy a))
            ]
        }

instance (HasRuby c) => HasRuby (Servant.ReqBody a b :> c) where
    type Ruby (Servant.ReqBody a b :> c) = Ruby c

    rubyFor _ endpoint = rubyFor (Proxy.Proxy :: Proxy.Proxy c) endpoint
        { endpointHasBody = True
        }

renderName :: Endpoint -> String
renderName endpoint =
    let method = renderMethod endpoint

        renderPathSegment (PathLiteral literal) = literal
        renderPathSegment (PathCapture capture) = capture
        renderPathSegment (PathMatrix (MatrixFlag flag)) = flag
        renderPathSegment (PathMatrix (MatrixParam param)) = param
        renderPathSegment (PathMatrix (MatrixParams params)) = params
        pathSegments =
            let segments = endpointPathSegments endpoint
                renderedSegments = map renderPathSegment segments
            in  if all isPathMatrix segments
                then "index" : renderedSegments
                else renderedSegments
        path = List.intercalate "_" pathSegments

        renderQueryItem (QueryFlag flag) = flag
        renderQueryItem (QueryParam param) = param
        renderQueryItem (QueryParams params) = params
        queryItems = map renderQueryItem (endpointQueryItems endpoint)
        query = if null queryItems
            then ""
            else "_" ++ List.intercalate "_" queryItems

    in  method ++ "_" ++ path ++ query

renderParams :: Endpoint -> String
renderParams endpoint =
    let renderPathSegment (PathLiteral _) = Nothing
        renderPathSegment (PathCapture capture) = Just capture
        renderPathSegment (PathMatrix (MatrixFlag flag)) = Just (flag ++ " = false")
        renderPathSegment (PathMatrix (MatrixParam param)) = Just (param ++ " = nil")
        renderPathSegment (PathMatrix (MatrixParams params)) = Just (params ++ " = []")
        pathSegments
            = endpoint
            |> endpointPathSegments
            |> map renderPathSegment
            |> Maybe.catMaybes

        renderQueryItem (QueryFlag flag) = Just (flag ++ " = false")
        renderQueryItem (QueryParam param) = Just (param ++ " = nil")
        renderQueryItem (QueryParams params) = Just (params ++ " = []")
        queryItems
            = endpoint
            |> endpointQueryItems
            |> Maybe.mapMaybe renderQueryItem

        body = if endpointHasBody endpoint
            then ["body = nil"]
            else []

    in  List.intercalate ", " (["excon"] ++ pathSegments ++ queryItems ++ body)

renderMethod :: Endpoint -> String
renderMethod endpoint = endpoint |> endpointMethod |> show |> map Char.toLower

renderPath :: Endpoint -> String
renderPath endpoint =
    let renderPathSegment (PathLiteral literal) = '/' : literal
        renderPathSegment (PathCapture capture) = concat ["/#{", capture, "}"]
        renderPathSegment (PathMatrix (MatrixFlag flag)) = concat ["#{';", flag, "' if ", flag, "}"]
        renderPathSegment (PathMatrix (MatrixParam param)) = concat [";", param, "=#{", param, "}"]
        renderPathSegment (PathMatrix (MatrixParams params)) = concat ["#{", params, ".map { |x| \";", params, "[]=#{x}\" }.join}"]
        pathSegments =
            let segments = endpointPathSegments endpoint
                renderedSegments = concatMap renderPathSegment segments
            in  if all isPathMatrix segments
                then '/' : renderedSegments
                else renderedSegments

        renderQueryItem (QueryFlag flag) = concat ["#{'&", flag, "' if ", flag, "}"]
        renderQueryItem (QueryParam param) = concat ["&", param, "=#{", param, "}"]
        renderQueryItem (QueryParams params) = concat ["#{", params, ".map { |x| \"&", params, "[]=#{x}\" }.join}"]
        queryItems = case endpointQueryItems endpoint of
            [] -> ""
            items -> '?' : concatMap renderQueryItem items

    in pathSegments ++ queryItems

class HasCode a where
    codeFor :: a -> String

instance HasCode Endpoint where
    codeFor endpoint = "\
        \def " ++ renderName endpoint ++ "(" ++ renderParams endpoint ++ ")\n\
        \  excon.request({\n\
        \    :method => :" ++ renderMethod endpoint ++ ",\n\
        \    :path => \"" ++ renderPath endpoint ++ "\",\n\
        \" ++ (if endpointHasBody endpoint then "    :body => body,\n" else "") ++ "\
        \  })\n\
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
