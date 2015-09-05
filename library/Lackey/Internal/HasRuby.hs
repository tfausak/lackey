{-# LANGUAGE TypeOperators #-}

module Lackey.Internal.HasRuby where

import Lackey.Internal.Endpoint
import Lackey.Internal.Header
import Lackey.Internal.MatrixItem
import Lackey.Internal.PathSegment
import Lackey.Internal.QueryItem
import Servant.API ((:<|>)(..))

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe

infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x

class HasRuby a where
    rubyFor :: a -> String

instance HasRuby () where
    rubyFor _ = ""

instance HasRuby Endpoint where
    rubyFor endpoint = "\
        \def " ++ renderName endpoint ++ "(" ++ renderParams endpoint ++ ")\n\
        \  excon.request(\n\
        \    method: :" ++ renderMethod endpoint ++ ",\n\
        \    path: \"" ++ renderPath endpoint ++ "\",\n\
        \    headers: {" ++ renderHeaders endpoint ++ "},\n\
        \    body: " ++ (if endpointHasBody endpoint then "body" else "nil") ++ "\n\
        \  )\n\
        \end\
    \"

instance (HasRuby a, HasRuby b) => HasRuby (a :<|> b) where
    rubyFor (x :<|> y) =
        let rx = rubyFor x
            ry = rubyFor y
            s = if null rx || null ry then "" else "\n\n"
        in  concat [rx, s, ry]

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

        renderHeader (Header x) = x
        headers = map renderHeader (endpointHeaders endpoint)
        header = if null headers
            then ""
            else "_" ++ List.intercalate "_" headers

    in  sanitize (method ++ "_" ++ path ++ query ++ header)

renderParams :: Endpoint -> String
renderParams endpoint =
    let renderPathSegment (PathCapture capture) = Just (sanitize capture)
        renderPathSegment _ = Nothing
        pathSegments
            = endpoint
            & endpointPathSegments
            & map renderPathSegment
            & Maybe.catMaybes

        renderMatrixItem (PathMatrix (MatrixFlag flag)) = Just (sanitize flag ++ ": false")
        renderMatrixItem (PathMatrix (MatrixParam param)) = Just (sanitize param ++ ": nil")
        renderMatrixItem (PathMatrix (MatrixParams params)) = Just (sanitize params ++ ": []")
        renderMatrixItem _ = Nothing
        matrixItems
            = endpoint
            & endpointPathSegments
            & Maybe.mapMaybe renderMatrixItem

        renderQueryItem (QueryFlag flag) = Just (sanitize flag ++ ": false")
        renderQueryItem (QueryParam param) = Just (sanitize param ++ ": nil")
        renderQueryItem (QueryParams params) = Just (params ++ ": []")
        queryItems
            = endpoint
            & endpointQueryItems
            & Maybe.mapMaybe renderQueryItem

        renderHeader (Header x) = x ++ ": nil"
        headers = endpoint & endpointHeaders & map renderHeader

        body = ["body" | endpointHasBody endpoint]

    in  List.intercalate ", "
        (concat [["excon"], pathSegments, body, matrixItems, queryItems, headers])

renderMethod :: Endpoint -> String
renderMethod endpoint = endpoint & endpointMethod & show & map Char.toLower

renderPath :: Endpoint -> String
renderPath endpoint =
    let renderPathSegment (PathLiteral literal) = '/' : literal
        renderPathSegment (PathCapture capture) = concat ["/#{", sanitize capture, "}"]
        renderPathSegment (PathMatrix (MatrixFlag flag)) = concat ["#{\";", flag, "\" if ", sanitize flag, "}"]
        renderPathSegment (PathMatrix (MatrixParam param)) = concat [";", param, "=#{", sanitize param, "}"]
        renderPathSegment (PathMatrix (MatrixParams params)) = concat ["#{", sanitize params, ".map { |x| \";", params, "[]=#{x}\" }.join}"]
        pathSegments =
            let segments = endpointPathSegments endpoint
                renderedSegments = concatMap renderPathSegment segments
            in  case segments of
                [] -> '/' : renderedSegments
                (PathMatrix _ : _) -> '/' : renderedSegments
                _ -> renderedSegments

        renderQueryItem (QueryFlag flag) = concat ["#{\"&", flag, "\" if ", sanitize flag, "}"]
        renderQueryItem (QueryParam param) = concat ["&", param, "=#{", sanitize param, "}"]
        renderQueryItem (QueryParams params) = concat ["#{", params, ".map { |x| \"&", params, "[]=#{x}\" }.join}"]
        queryItems = case endpointQueryItems endpoint of
            [] -> ""
            items -> '?' : concatMap renderQueryItem items

    in pathSegments ++ queryItems

renderHeaders :: Endpoint -> String
renderHeaders endpoint =
    let headers = endpointHeaders endpoint
    in  if null headers
        then ""
        else
            let y = headers
                    & map (\ (Header x) -> concat ["\"", x, "\" => ", x])
                    & List.intercalate ", "
            in  " " ++ y ++ " "

sanitize :: String -> String
sanitize xs
    = xs
    & map (\ x -> if Char.isAlphaNum x then Char.toLower x else '_')
