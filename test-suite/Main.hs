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

spec :: Spec
spec = do
    describe "rubyForAPI" $ do
        it "generates a function for a delete request" $ do
            let api = Proxy :: Proxy (Delete '[] ())
            rubyForAPI api `shouldBe` "\
                \def delete_index(excon)\n\
                \  excon.request(\n\
                \    method: :delete,\n\
                \    path: \"/\",\n\
                \    headers: {},\n\
                \    body: nil\n\
                \  )\n\
                \end\
            \"

        it "generates a function for a get request" $ do
            let api = Proxy :: Proxy (Get '[] ())
            rubyForAPI api `shouldBe` "\
                \def get_index(excon)\n\
                \  excon.request(\n\
                \    method: :get,\n\
                \    path: \"/\",\n\
                \    headers: {},\n\
                \    body: nil\n\
                \  )\n\
                \end\
            \"

        it "generates a function for a patch request" $ do
            let api = Proxy :: Proxy (Patch '[] ())
            rubyForAPI api `shouldBe` "\
                \def patch_index(excon)\n\
                \  excon.request(\n\
                \    method: :patch,\n\
                \    path: \"/\",\n\
                \    headers: {},\n\
                \    body: nil\n\
                \  )\n\
                \end\
            \"

        it "generates a function for a post request" $ do
            let api = Proxy :: Proxy (Post '[] ())
            rubyForAPI api `shouldBe` "\
                \def post_index(excon)\n\
                \  excon.request(\n\
                \    method: :post,\n\
                \    path: \"/\",\n\
                \    headers: {},\n\
                \    body: nil\n\
                \  )\n\
                \end\
            \"

        it "generates a function for a put request" $ do
            let api = Proxy :: Proxy (Put '[] ())
            rubyForAPI api `shouldBe` "\
                \def put_index(excon)\n\
                \  excon.request(\n\
                \    method: :put,\n\
                \    path: \"/\",\n\
                \    headers: {},\n\
                \    body: nil\n\
                \  )\n\
                \end\
            \"

        it "generates a function for a resource" $ do
            let api = Proxy :: Proxy ("resource" :> Get '[] ())
            rubyForAPI api `shouldBe` "\
                \def get_resource(excon)\n\
                \  excon.request(\n\
                \    method: :get,\n\
                \    path: \"/resource\",\n\
                \    headers: {},\n\
                \    body: nil\n\
                \  )\n\
                \end\
            \"

        it "generates functions for two endpoints" $ do
            let api = Proxy :: Proxy (Get '[] () :<|> Delete '[] ())
            rubyForAPI api `shouldBe` "\
                \def get_index(excon)\n\
                \  excon.request(\n\
                \    method: :get,\n\
                \    path: \"/\",\n\
                \    headers: {},\n\
                \    body: nil\n\
                \  )\n\
                \end\n\
                \\n\
                \def delete_index(excon)\n\
                \  excon.request(\n\
                \    method: :delete,\n\
                \    path: \"/\",\n\
                \    headers: {},\n\
                \    body: nil\n\
                \  )\n\
                \end\
            \"

        it "generates a function for a capture" $ do
            let api = Proxy :: Proxy (Capture "id" () :> Get '[] ())
            rubyForAPI api `shouldBe` "\
                \def get_id(excon, id)\n\
                \  excon.request(\n\
                \    method: :get,\n\
                \    path: \"/#{id}\",\n\
                \    headers: {},\n\
                \    body: nil\n\
                \  )\n\
                \end\
            \"

        it "generates a function for a matrix flag" $ do
            let api = Proxy :: Proxy (MatrixFlag "flag" :> Get '[] ())
            rubyForAPI api `shouldBe` "\
                \def get_index_flag(excon, flag: false)\n\
                \  excon.request(\n\
                \    method: :get,\n\
                \    path: \"/#{';flag' if flag}\",\n\
                \    headers: {},\n\
                \    body: nil\n\
                \  )\n\
                \end\
            \"

        it "generates a function for a matrix param" $ do
            let api = Proxy :: Proxy (MatrixParam "param" () :> Get '[] ())
            rubyForAPI api `shouldBe` "\
                \def get_index_param(excon, param: nil)\n\
                \  excon.request(\n\
                \    method: :get,\n\
                \    path: \"/;param=#{param}\",\n\
                \    headers: {},\n\
                \    body: nil\n\
                \  )\n\
                \end\
            \"

        it "generates a function for a matrix params" $ do
            let api = Proxy :: Proxy (MatrixParams "params" () :> Get '[] ())
            rubyForAPI api `shouldBe` "\
                \def get_index_params(excon, params: [])\n\
                \  excon.request(\n\
                \    method: :get,\n\
                \    path: \"/#{params.map { |x| \";params[]=#{x}\" }.join}\",\n\
                \    headers: {},\n\
                \    body: nil\n\
                \  )\n\
                \end\
            \"

        it "always prepends a slash" $ do
            let api = Proxy :: Proxy (MatrixFlag "a" :> "b" :> Get '[] ())
            rubyForAPI api `shouldBe` "\
                \def get_a_b(excon, a: false)\n\
                \  excon.request(\n\
                \    method: :get,\n\
                \    path: \"/#{';a' if a}/b\",\n\
                \    headers: {},\n\
                \    body: nil\n\
                \  )\n\
                \end\
            \"

        it "generates a function for a query flag" $ do
            let api = Proxy :: Proxy (QueryFlag "flag" :> Get '[] ())
            rubyForAPI api `shouldBe` "\
                \def get_index_flag(excon, flag: false)\n\
                \  excon.request(\n\
                \    method: :get,\n\
                \    path: \"/?#{'&flag' if flag}\",\n\
                \    headers: {},\n\
                \    body: nil\n\
                \  )\n\
                \end\
            \"

        it "generates a function for a query param" $ do
            let api = Proxy :: Proxy (QueryParam "param" () :> Get '[] ())
            rubyForAPI api `shouldBe` "\
                \def get_index_param(excon, param: nil)\n\
                \  excon.request(\n\
                \    method: :get,\n\
                \    path: \"/?&param=#{param}\",\n\
                \    headers: {},\n\
                \    body: nil\n\
                \  )\n\
                \end\
            \"

        it "generates a function for a query params" $ do
            let api = Proxy :: Proxy (QueryParams "params" () :> Get '[] ())
            rubyForAPI api `shouldBe` "\
                \def get_index_params(excon, params: [])\n\
                \  excon.request(\n\
                \    method: :get,\n\
                \    path: \"/?#{params.map { |x| \"&params[]=#{x}\" }.join}\",\n\
                \    headers: {},\n\
                \    body: nil\n\
                \  )\n\
                \end\
            \"

        it "generates a function for a request body" $ do
            let api = Proxy :: Proxy (ReqBody '[] () :> Get '[] ())
            rubyForAPI api `shouldBe` "\
                \def get_index(excon, body)\n\
                \  excon.request(\n\
                \    method: :get,\n\
                \    path: \"/\",\n\
                \    headers: {},\n\
                \    body: body\n\
                \  )\n\
                \end\
            \"

        it "puts the body after path segments" $ do
            let api = Proxy :: Proxy (Capture "segment" () :> ReqBody '[] () :> Get '[] ())
            rubyForAPI api `shouldBe` "\
                \def get_segment(excon, segment, body)\n\
                \  excon.request(\n\
                \    method: :get,\n\
                \    path: \"/#{segment}\",\n\
                \    headers: {},\n\
                \    body: body\n\
                \  )\n\
                \end\
            \"

        it "puts the body before query params" $ do
            let api = Proxy :: Proxy (QueryFlag "flag" :> ReqBody '[] () :> Get '[] ())
            rubyForAPI api `shouldBe` "\
                \def get_index_flag(excon, body, flag: false)\n\
                \  excon.request(\n\
                \    method: :get,\n\
                \    path: \"/?#{'&flag' if flag}\",\n\
                \    headers: {},\n\
                \    body: body\n\
                \  )\n\
                \end\
            \"

        it "puts the body before matrix params" $ do
            let api = Proxy :: Proxy (MatrixFlag "flag" :> ReqBody '[] () :> Get '[] ())
            rubyForAPI api `shouldBe` "\
                \def get_index_flag(excon, body, flag: false)\n\
                \  excon.request(\n\
                \    method: :get,\n\
                \    path: \"/#{';flag' if flag}\",\n\
                \    headers: {},\n\
                \    body: body\n\
                \  )\n\
                \end\
            \"

        it "generates a function for a header" $ do
            let api = Proxy :: Proxy (Header "cookie" () :> Get '[] ())
            rubyForAPI api `shouldBe` "\
                \def get_index_cookie(excon, cookie: nil)\n\
                \  excon.request(\n\
                \    method: :get,\n\
                \    path: \"/\",\n\
                \    headers: { \"cookie\" => cookie },\n\
                \    body: nil\n\
                \  )\n\
                \end\
            \"

        it "supports response headers" $ do
            let api = Proxy :: Proxy (Get '[] (Headers '[] ()))
            rubyForAPI api `shouldBe` "\
                \def get_index(excon)\n\
                \  excon.request(\n\
                \    method: :get,\n\
                \    path: \"/\",\n\
                \    headers: {},\n\
                \    body: nil\n\
                \  )\n\
                \end\
            \"
