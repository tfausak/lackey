{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Lackey
  ( rubyForAPI
  ) where

import qualified Data.Char as Char
import Data.Function ((&))
import qualified Data.Maybe as Maybe
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
renderRequests requests = requests & fmap renderRequest & Text.intercalate ";"

functionName :: Servant.Req Request -> Text.Text
functionName request = request & Servant._reqFuncName & Servant.snakeCase

hasBody :: Servant.Req Request -> Bool
hasBody request = case Servant._reqBody request of
  Nothing -> False
  Just _ -> True

bodyArgument :: Text.Text
bodyArgument = "body"

underscore :: Text.Text -> Text.Text
underscore text =
  text & Text.toLower & Text.map (\c -> if Char.isAlphaNum c then c else '_')

getHeaders :: Servant.Req Request -> [Text.Text]
getHeaders request =
  request
    & Servant._reqHeaders
    & Maybe.mapMaybe
        (\h -> case h of
          Servant.HeaderArg x -> Just x
          Servant.ReplaceHeaderArg _ _ -> Nothing
        )
    & fmap (Servant.unPathSegment . Servant._argName)

getURLPieces :: Servant.Req Request -> [Either Text.Text Text.Text]
getURLPieces request =
  let
    url = request & Servant._reqUrl
    path =
      url
        & Servant._path
        & Maybe.mapMaybe
            ((\segment -> case segment of
               Servant.Static _ -> Nothing
               Servant.Cap arg -> Just arg
             )
            . Servant.unSegment
            )
        & fmap (Servant.unPathSegment . Servant._argName)
    query = url & Servant._queryStr & fmap
      (Servant.unPathSegment . Servant._argName . Servant._queryArgName)
  in fmap Left path <> fmap Right query

functionArguments :: Servant.Req Request -> Text.Text
functionArguments request = Text.concat
  [ "("
  , [ [Just "excon"]
    , request & getURLPieces & fmap
      (\piece -> Just $ case piece of
        Left capture -> underscore capture
        Right param -> underscore param <> ": nil"
      )
    , request & getHeaders & fmap (Just . (<> ": nil") . underscore)
    , [if hasBody request then Just bodyArgument else Nothing]
    ]
  & concat
  & Maybe.catMaybes
  & Text.intercalate ","
  , ")"
  ]

requestMethod :: Servant.Req Request -> Text.Text
requestMethod request =
  request & Servant._reqMethod & Text.decodeUtf8 & Text.toLower & Text.cons ':'

requestPath :: Servant.Req Request -> Text.Text
requestPath request =
  let
    path =
      request
        & Servant._reqUrl
        & Servant._path
        & fmap
            ((\x -> case x of
               Servant.Static y -> Servant.unPathSegment y
               Servant.Cap y ->
                 let
                   z =
                     y & Servant._argName & Servant.unPathSegment & underscore
                 in "#{" <> z <> "}"
             )
            . Servant.unSegment
            )
        & Text.intercalate "/"
    query =
      request
        & Servant._reqUrl
        & Servant._queryStr
        & fmap
            ((\x -> x <> "=#{" <> underscore x <> "}")
            . Servant.unPathSegment
            . Servant._argName
            . Servant._queryArgName
            )
        & Text.intercalate "&"
    url = "/" <> path <> (if Text.null query then "" else "?" <> query)
  in "\"" <> url <> "\""

requestHeaders :: Servant.Req Request -> Text.Text
requestHeaders request =
  [ ["{"]
    , request & getHeaders & fmap (\x -> "\"" <> x <> "\"=>" <> underscore x)
    , ["}"]
    ]
    & concat
    & Text.concat

requestBody :: Servant.Req Request -> Text.Text
requestBody request = if hasBody request then bodyArgument else "nil"

functionBody :: Servant.Req Request -> Text.Text
functionBody request = Text.concat
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
renderRequest request = Text.concat
  [ "def "
  , functionName request
  , functionArguments request
  , functionBody request
  , "end"
  ]

requestsForAPI
  :: ( Servant.HasForeign Language Request api
     , Servant.GenerateList Request (Servant.Foreign Request api)
     )
  => Proxy.Proxy api
  -> [Servant.Req Request]
requestsForAPI api = api & Servant.listFromAPI languageProxy requestProxy

rubyForAPI
  :: ( Servant.HasForeign Language Request api
     , Servant.GenerateList Request (Servant.Foreign Request api)
     )
  => Proxy.Proxy api
  -> Text.Text
rubyForAPI api = api & requestsForAPI & renderRequests
