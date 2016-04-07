{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Lackey (rubyForAPI) where

import Data.Function ((&))
import qualified Data.Proxy as Proxy
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Servant.Foreign as Servant

type Language = Servant.NoTypes

languageProxy :: Proxy.Proxy Language
languageProxy = Proxy.Proxy

type Request = ()

requestProxy :: Proxy.Proxy Request
requestProxy = Proxy.Proxy

renderRequests :: [Servant.Req Request] -> Text.Text
renderRequests requests = requests & map renderRequest & Text.intercalate ";"

functionName :: Servant.Req Request -> Text.Text
functionName request = request & Servant._reqFuncName & Servant.snakeCase

functionArguments :: Servant.Req Request -> Text.Text
functionArguments _ = "(excon)"

requestMethod :: Servant.Req Request -> Text.Text
requestMethod request = request & Servant._reqMethod & Text.decodeUtf8 & Text.toLower & Text.cons ':'

requestPath :: Servant.Req Request -> Text.Text
requestPath _request = "''"

requestHeaders :: Servant.Req Request -> Text.Text
requestHeaders _request = "{}"

functionBody :: Servant.Req Request -> Text.Text
functionBody request = Text.concat
    [ "excon.request("
    , ":method=>", requestMethod request, ","
    , ":path=>", requestPath request, ","
    , ":headers=>", requestHeaders request, ","
    , ":body=>nil"
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

requestsForAPI :: (Servant.HasForeign Language Request api, Servant.GenerateList Request (Servant.Foreign Request api)) => Proxy.Proxy api -> [Servant.Req Request]
requestsForAPI api = api & Servant.listFromAPI languageProxy requestProxy

rubyForAPI :: (Servant.HasForeign Language Request api, Servant.GenerateList Request (Servant.Foreign Request api)) => Proxy.Proxy api -> Text.Text
rubyForAPI api = api & requestsForAPI & renderRequests
