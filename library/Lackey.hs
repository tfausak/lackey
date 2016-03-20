{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Lackey (rubyForAPI) where

import qualified Data.Proxy as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Servant.Foreign as S

type Language = S.NoTypes

languageProxy :: P.Proxy Language
languageProxy = P.Proxy

type Request = ()

requestProxy :: P.Proxy Request
requestProxy = P.Proxy

renderRequests :: [S.Req Request] -> T.Text
renderRequests requests = T.intercalate ";" (map renderRequest requests)

functionName :: S.Req Request -> T.Text
functionName request = S.snakeCase (S._reqFuncName request)

functionArguments :: S.Req Request -> T.Text
functionArguments _ = "(excon)"

functionBody :: S.Req Request -> T.Text
functionBody request = T.concat
    [ "excon.request("
    , ":method=>:", T.toLower (E.decodeUtf8 (S._reqMethod request)), ","
    , ":path=>'',"
    , ":headers=>{},"
    , ":body=>nil"
    , ")"
    ]

renderRequest :: S.Req Request -> T.Text
renderRequest request = T.concat
    [ "def "
    , functionName request
    , functionArguments request
    , functionBody request
    , "end"
    ]

requestsForAPI :: (S.HasForeign Language Request api, S.GenerateList Request (S.Foreign Request api)) => P.Proxy api -> [S.Req Request]
requestsForAPI api = S.listFromAPI languageProxy requestProxy api

rubyForAPI :: (S.HasForeign Language Request api, S.GenerateList Request (S.Foreign Request api)) => P.Proxy api -> T.Text
rubyForAPI api = renderRequests (requestsForAPI api)
