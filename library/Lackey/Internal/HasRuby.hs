{-# LANGUAGE TypeOperators #-}

module Lackey.Internal.HasRuby where

import Flow
import Lackey.Internal.Endpoint
import Lackey.Internal.MatrixItem
import Lackey.Internal.PathSegment
import Lackey.Internal.QueryItem
import Servant.API ((:<|>)(..))

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe

class HasRuby a where
    rubyFor :: a -> String

instance HasRuby Endpoint where
    rubyFor endpoint = "\
        \def " ++ renderName endpoint ++ "(" ++ renderParams endpoint ++ ")\n\
        \  excon.request({\n\
        \    method: :" ++ renderMethod endpoint ++ ",\n\
        \    path: \"" ++ renderPath endpoint ++ "\",\n\
        \    body: " ++ (if endpointHasBody endpoint then "body" else "nil") ++ ",\n\
        \  })\n\
        \end\
    \"

instance (HasRuby a, HasRuby b) => HasRuby (a :<|> b) where
    rubyFor (x :<|> y) = concat [rubyFor x, "\n\n", rubyFor y]

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
    let renderPathSegment (PathCapture capture) = Just capture
        renderPathSegment _ = Nothing
        pathSegments
            = endpoint
            |> endpointPathSegments
            |> map renderPathSegment
            |> Maybe.catMaybes

        renderMatrixItem (PathMatrix (MatrixFlag flag)) = Just (flag ++ ": false")
        renderMatrixItem (PathMatrix (MatrixParam param)) = Just (param ++ ": nil")
        renderMatrixItem (PathMatrix (MatrixParams params)) = Just (params ++ ": []")
        renderMatrixItem _ = Nothing
        matrixItems
            = endpoint
            |> endpointPathSegments
            |> Maybe.mapMaybe renderMatrixItem

        renderQueryItem (QueryFlag flag) = Just (flag ++ ": false")
        renderQueryItem (QueryParam param) = Just (param ++ ": nil")
        renderQueryItem (QueryParams params) = Just (params ++ ": []")
        queryItems
            = endpoint
            |> endpointQueryItems
            |> Maybe.mapMaybe renderQueryItem

        body = ["body" | endpointHasBody endpoint]

    in  List.intercalate ", "
        (concat [["excon"], pathSegments, body, matrixItems, queryItems])

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
            in  case segments of
                [] -> '/' : renderedSegments
                (PathMatrix _ : _) -> '/' : renderedSegments
                _ -> renderedSegments

        renderQueryItem (QueryFlag flag) = concat ["#{'&", flag, "' if ", flag, "}"]
        renderQueryItem (QueryParam param) = concat ["&", param, "=#{", param, "}"]
        renderQueryItem (QueryParams params) = concat ["#{", params, ".map { |x| \"&", params, "[]=#{x}\" }.join}"]
        queryItems = case endpointQueryItems endpoint of
            [] -> ""
            items -> '?' : concatMap renderQueryItem items

    in pathSegments ++ queryItems
