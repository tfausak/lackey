{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Lackey
  ( rubyForAPI
  ) where

import qualified Data.Char as Char
import Data.Function ((&))
import qualified Data.Maybe as Maybe
import Data.Monoid ((<>))
import qualified Data.Proxy as Proxy
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Servant.Foreign as Servant

type Language = Servant.NoTypes

languageProxy :: Proxy.Proxy Language
languageProxy = Proxy.Proxy

type Request = Servant.NoContent

requestProxy :: Proxy.Proxy Request
requestProxy = Proxy.Proxy

renderRequests :: [Servant.Req Request] -> Text.Text
renderRequests requests = requests & map renderRequest & Text.intercalate ";"

functionName :: Servant.Req Request -> Text.Text
functionName request = request & Servant._reqFuncName & Servant.snakeCase

hasBody :: Servant.Req Request -> Bool
hasBody request =
  case Servant._reqBody request of
    Nothing -> False
    Just _ -> True

bodyArgument :: Text.Text
bodyArgument = "body"

underscore :: Text.Text -> Text.Text
underscore text =
  text & Text.toLower &
  Text.map
    (\c ->
       if Char.isAlphaNum c
         then c
         else '_')

getHeaders :: Servant.Req Request -> [Text.Text]
getHeaders request =
  request & Servant._reqHeaders &
  Maybe.mapMaybe
    (\h ->
       case h of
         Servant.HeaderArg x -> Just x
         Servant.ReplaceHeaderArg _ _ -> Nothing) &
  map Servant._argName &
  map Servant.unPathSegment

getURLPieces :: Servant.Req Request -> [Either Text.Text Text.Text]
getURLPieces request =
  let url = request & Servant._reqUrl
      path =
        url & Servant._path & map Servant.unSegment &
        Maybe.mapMaybe
          (\segment ->
             case segment of
               Servant.Static _ -> Nothing
               Servant.Cap arg -> Just arg) &
        map Servant._argName &
        map Servant.unPathSegment
      query =
        url & Servant._queryStr & map Servant._queryArgName &
        map Servant._argName &
        map Servant.unPathSegment
  in map Left path ++ map Right query

functionArguments :: Servant.Req Request -> Text.Text
functionArguments request =
  Text.concat
    [ "("
    , [ [Just "excon"]
      , request & getURLPieces &
        map
          (\piece ->
             case piece of
               Left capture -> underscore capture
               Right param -> underscore param <> ": nil") &
        map Just
      , request & getHeaders & map underscore & map (<> ": nil") & map Just
      , [ if hasBody request
            then Just bodyArgument
            else Nothing
        ]
      ] &
      concat &
      Maybe.catMaybes &
      Text.intercalate ","
    , ")"
    ]

requestMethod :: Servant.Req Request -> Text.Text
requestMethod request =
  request & Servant._reqMethod & Text.decodeUtf8 & Text.toLower & Text.cons ':'

requestPath :: Servant.Req Request -> Text.Text
requestPath request =
  let path =
        request & Servant._reqUrl & Servant._path & map Servant.unSegment &
        map
          (\x ->
             case x of
               Servant.Static y -> Servant.unPathSegment y
               Servant.Cap y ->
                 let z =
                       y & Servant._argName & Servant.unPathSegment & underscore
                 in "#{" <> z <> "}") &
        Text.intercalate "/"
      query =
        request & Servant._reqUrl & Servant._queryStr &
        map Servant._queryArgName &
        map Servant._argName &
        map Servant.unPathSegment &
        map (\x -> x <> "=#{" <> underscore x <> "}") &
        Text.intercalate "&"
      url =
        "/" <> path <>
        (if Text.null query
           then ""
           else "?" <> query)
  in "\"" <> url <> "\""

requestHeaders :: Servant.Req Request -> Text.Text
requestHeaders request =
  [ ["{"]
  , request & getHeaders & map (\x -> "\"" <> x <> "\"=>" <> underscore x)
  , ["}"]
  ] &
  concat &
  Text.concat

requestBody :: Servant.Req Request -> Text.Text
requestBody request =
  if hasBody request
    then bodyArgument
    else "nil"

functionBody :: Servant.Req Request -> Text.Text
functionBody request =
  Text.concat
    [ "excon.request("
    , ":method=>"
    , requestMethod request
    , ","
    , ":path=>"
    , requestPath request
    , ","
    , ":headers=>"
    , requestHeaders request
    , ","
    , ":body=>"
    , requestBody request
    , ")"
    ]

renderRequest :: Servant.Req Request -> Text.Text
renderRequest request =
  Text.concat
    [ "def "
    , functionName request
    , functionArguments request
    , functionBody request
    , "end"
    ]

requestsForAPI ::
     ( Servant.HasForeign Language Request api
     , Servant.GenerateList Request (Servant.Foreign Request api)
     )
  => Proxy.Proxy api
  -> [Servant.Req Request]
requestsForAPI api = api & Servant.listFromAPI languageProxy requestProxy

rubyForAPI ::
     ( Servant.HasForeign Language Request api
     , Servant.GenerateList Request (Servant.Foreign Request api)
     )
  => Proxy.Proxy api
  -> Text.Text
rubyForAPI api = api & requestsForAPI & renderRequests
