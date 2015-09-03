{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Proxy
import Lackey
import Servant.API
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = defaultMain =<< testSpec "Lackey" spec

-- Since every generated function has the same structure, it can be abstracted
-- away behind this function.
ruby :: String -> String -> String -> String -> String -> String -> String
ruby name params method path headers body = "\
    \def " ++ name ++ "(excon" ++ params ++ ")\n\
    \  excon.request(\n\
    \    method: :" ++ method ++ ",\n\
    \    path: \"/" ++ path ++ "\",\n\
    \    headers: {" ++ headers ++ "},\n\
    \    body: " ++ body ++ "\n\
    \  )\n\
    \end\
\"

spec :: Spec
spec = do
    describe "rubyForAPI" $ do
        it "generates a function for a delete request" $ do
            let api = Proxy :: Proxy (Delete '[] ())
            rubyForAPI api `shouldBe`
                ruby "delete_index" "" "delete" "" "" "nil"

        it "generates a function for a get request" $ do
            let api = Proxy :: Proxy (Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_index" "" "get" "" "" "nil"

        it "generates a function for a patch request" $ do
            let api = Proxy :: Proxy (Patch '[] ())
            rubyForAPI api `shouldBe`
                ruby "patch_index" "" "patch" "" "" "nil"

        it "generates a function for a post request" $ do
            let api = Proxy :: Proxy (Post '[] ())
            rubyForAPI api `shouldBe`
                ruby "post_index" "" "post" "" "" "nil"

        it "generates a function for a put request" $ do
            let api = Proxy :: Proxy (Put '[] ())
            rubyForAPI api `shouldBe`
                ruby "put_index" "" "put" "" "" "nil"

        it "generates a function for a resource" $ do
            let api = Proxy :: Proxy ("resource" :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_resource" "" "get" "resource" "" "nil"

        it "generates functions for two endpoints" $ do
            let api = Proxy :: Proxy (Get '[] () :<|> Delete '[] ())
            rubyForAPI api `shouldBe` concat
                [ ruby "get_index" "" "get" "" "" "nil"
                , "\n\n"
                , ruby "delete_index" "" "delete" "" "" "nil"
                ]

        it "generates a function for a capture" $ do
            let api = Proxy :: Proxy (Capture "id" () :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_id" ", id" "get" "#{id}" "" "nil"

        it "generates a function for a matrix flag" $ do
            let api = Proxy :: Proxy (MatrixFlag "flag" :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_index_flag" ", flag: false" "get" "#{';flag' if flag}" "" "nil"

        it "generates a function for a matrix param" $ do
            let api = Proxy :: Proxy (MatrixParam "param" () :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_index_param" ", param: nil" "get" ";param=#{param}" "" "nil"

        it "generates a function for a matrix params" $ do
            let api = Proxy :: Proxy (MatrixParams "params" () :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_index_params" ", params: []" "get" "#{params.map { |x| \";params[]=#{x}\" }.join}" "" "nil"

        it "always prepends a slash" $ do
            let api = Proxy :: Proxy (MatrixFlag "a" :> "b" :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_a_b" ", a: false" "get" "#{';a' if a}/b" "" "nil"

        it "generates a function for a query flag" $ do
            let api = Proxy :: Proxy (QueryFlag "flag" :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_index_flag" ", flag: false" "get" "?#{'&flag' if flag}" "" "nil"

        it "generates a function for a query param" $ do
            let api = Proxy :: Proxy (QueryParam "param" () :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_index_param" ", param: nil" "get" "?&param=#{param}" "" "nil"

        it "generates a function for a query params" $ do
            let api = Proxy :: Proxy (QueryParams "params" () :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_index_params" ", params: []" "get" "?#{params.map { |x| \"&params[]=#{x}\" }.join}" "" "nil"

        it "generates a function for a request body" $ do
            let api = Proxy :: Proxy (ReqBody '[] () :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_index" ", body" "get" "" "" "body"

        it "puts the body after path segments" $ do
            let api = Proxy :: Proxy (Capture "segment" () :> ReqBody '[] () :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_segment" ", segment, body" "get" "#{segment}" "" "body"

        it "puts the body before query params" $ do
            let api = Proxy :: Proxy (QueryFlag "flag" :> ReqBody '[] () :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_index_flag" ", body, flag: false" "get" "?#{'&flag' if flag}" "" "body"

        it "puts the body before matrix params" $ do
            let api = Proxy :: Proxy (MatrixFlag "flag" :> ReqBody '[] () :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_index_flag" ", body, flag: false" "get" "#{';flag' if flag}" "" "body"

        it "generates a function for a header" $ do
            let api = Proxy :: Proxy (Header "cookie" () :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_index_cookie" ", cookie: nil" "get" "" " \"cookie\" => cookie " "nil"

        it "supports response headers" $ do
            let api = Proxy :: Proxy (Get '[] (Headers '[] ()))
            rubyForAPI api `shouldBe`
                ruby "get_index" "" "get" "" "" "nil"
