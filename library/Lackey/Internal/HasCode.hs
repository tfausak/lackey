{-# LANGUAGE TypeOperators #-}

module Lackey.Internal.HasCode where

import Flow
import Lackey.Internal.Endpoint
import Lackey.Internal.MatrixItem
import Lackey.Internal.PathSegment
import Lackey.Internal.QueryItem
import Servant.API ((:<|>)(..))

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe

class HasCode a where
    codeFor :: a -> String

instance HasCode Endpoint where
    codeFor endpoint = "\
        \def " ++ renderName endpoint ++ "(" ++ renderParams endpoint ++ ")\n\
        \  excon.request({\n\
        \    method: :" ++ renderMethod endpoint ++ ",\n\
        \    path: \"" ++ renderPath endpoint ++ "\",\n\
        \" ++ (if endpointHasBody endpoint then "    body: body,\n" else "") ++ "\
        \  })\n\
        \end\
    \"

instance (HasCode a, HasCode b) => HasCode (a :<|> b) where
    codeFor (x :<|> y) = concat [codeFor x, "\n\n", codeFor y]

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
        renderPathSegment (PathMatrix (MatrixFlag flag)) = Just (flag ++ ": false")
        renderPathSegment (PathMatrix (MatrixParam param)) = Just (param ++ ": nil")
        renderPathSegment (PathMatrix (MatrixParams params)) = Just (params ++ ": []")
        pathSegments
            = endpoint
            |> endpointPathSegments
            |> map renderPathSegment
            |> Maybe.catMaybes

        renderQueryItem (QueryFlag flag) = Just (flag ++ ": false")
        renderQueryItem (QueryParam param) = Just (param ++ ": nil")
        renderQueryItem (QueryParams params) = Just (params ++ ": []")
        queryItems
            = endpoint
            |> endpointQueryItems
            |> Maybe.mapMaybe renderQueryItem

        body = ["body = nil" | endpointHasBody endpoint]

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
