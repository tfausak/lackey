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
        it "supports delete requests" $ do
            let api = Proxy :: Proxy (Delete '[] ())
            rubyForAPI api `shouldBe`
                ruby "delete_index" "" "delete" "" "" "nil"

        it "supports get requests" $ do
            let api = Proxy :: Proxy (Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_index" "" "get" "" "" "nil"

        it "supports patch requests" $ do
            let api = Proxy :: Proxy (Patch '[] ())
            rubyForAPI api `shouldBe`
                ruby "patch_index" "" "patch" "" "" "nil"

        it "supports post requests" $ do
            let api = Proxy :: Proxy (Post '[] ())
            rubyForAPI api `shouldBe`
                ruby "post_index" "" "post" "" "" "nil"

        it "supports put requests" $ do
            let api = Proxy :: Proxy (Put '[] ())
            rubyForAPI api `shouldBe`
                ruby "put_index" "" "put" "" "" "nil"

        it "supports path components" $ do
            let api = Proxy :: Proxy ("resource" :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_resource" "" "get" "resource" "" "nil"

        it "supports alternatives" $ do
            let api = Proxy :: Proxy (Get '[] () :<|> Delete '[] ())
            rubyForAPI api `shouldBe` concat
                [ ruby "get_index" "" "get" "" "" "nil"
                , "\n\n"
                , ruby "delete_index" "" "delete" "" "" "nil"
                ]

        it "supports captures" $ do
            let api = Proxy :: Proxy (Capture "id" () :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_id" ", id" "get" "#{id}" "" "nil"

        it "supports matrix flags" $ do
            let api = Proxy :: Proxy (MatrixFlag "flag" :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_index_flag" ", flag: false" "get" "#{\";flag\" if flag}" "" "nil"

        it "supports matrix params" $ do
            let api = Proxy :: Proxy (MatrixParam "param" () :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_index_param" ", param: nil" "get" ";param=#{param}" "" "nil"

        it "supports multiple matrix params" $ do
            let api = Proxy :: Proxy (MatrixParams "params" () :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_index_params" ", params: []" "get" "#{params.map { |x| \";params[]=#{x}\" }.join}" "" "nil"

        it "supports query flags" $ do
            let api = Proxy :: Proxy (QueryFlag "flag" :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_index_flag" ", flag: false" "get" "?#{\"&flag\" if flag}" "" "nil"

        it "supports query params" $ do
            let api = Proxy :: Proxy (QueryParam "param" () :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_index_param" ", param: nil" "get" "?&param=#{param}" "" "nil"

        it "supports multiple query params" $ do
            let api = Proxy :: Proxy (QueryParams "params" () :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_index_params" ", params: []" "get" "?#{params.map { |x| \"&params[]=#{x}\" }.join}" "" "nil"

        it "supports request bodies" $ do
            let api = Proxy :: Proxy (ReqBody '[] () :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_index" ", body" "get" "" "" "body"

        it "supports request headers" $ do
            let api = Proxy :: Proxy (Header "cookie" () :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_index_cookie" ", cookie: nil" "get" "" " \"cookie\" => cookie " "nil"

        it "supports response headers" $ do
            let api = Proxy :: Proxy (Get '[] (Headers '[] ()))
            rubyForAPI api `shouldBe`
                ruby "get_index" "" "get" "" "" "nil"

        it "supports raw" $ do
            let api = Proxy :: Proxy Raw
            rubyForAPI api `shouldBe` ""

        it "supports raw as part of a larger API" $ do
            let api = Proxy :: Proxy (Get '[] () :<|> Raw)
            rubyForAPI api `shouldBe`
                ruby "get_index" "" "get" "" "" "nil"

        it "always starts the path with a slash" $ do
            let api = Proxy :: Proxy (MatrixFlag "a" :> "b" :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_a_b" ", a: false" "get" "#{\";a\" if a}/b" "" "nil"

        it "puts the body param after captures" $ do
            let api = Proxy :: Proxy (Capture "segment" () :> ReqBody '[] () :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_segment" ", segment, body" "get" "#{segment}" "" "body"

        it "puts the body param before query params" $ do
            let api = Proxy :: Proxy (QueryFlag "flag" :> ReqBody '[] () :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_index_flag" ", body, flag: false" "get" "?#{\"&flag\" if flag}" "" "body"

        it "puts the body param before matrix params" $ do
            let api = Proxy :: Proxy (MatrixFlag "flag" :> ReqBody '[] () :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_index_flag" ", body, flag: false" "get" "#{\";flag\" if flag}" "" "body"

        it "sanitizes path segments" $ do
            let api = Proxy :: Proxy ("A!" :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_a_" "" "get" "A!" "" "nil"

        it "sanitizes captures" $ do
            let api = Proxy :: Proxy (Capture "A!" () :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_a_" ", a_" "get" "#{a_}" "" "nil"

        it "sanitizes matrix flags" $ do
            let api = Proxy :: Proxy (MatrixFlag "A!" :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_index_a_" ", a_: false" "get" "#{\";A!\" if a_}" "" "nil"

        it "sanitizes matrix params" $ do
            let api = Proxy :: Proxy (MatrixParam "A!" () :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_index_a_" ", a_: nil" "get" ";A!=#{a_}" "" "nil"

        it "sanitizes multiple matrix params" $ do
            let api = Proxy :: Proxy (MatrixParams "A!" () :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_index_a_" ", a_: []" "get" "#{a_.map { |x| \";A![]=#{x}\" }.join}" "" "nil"

        it "sanitizes query flags" $ do
            let api = Proxy :: Proxy (QueryFlag "A!" :> Get '[] ())
            rubyForAPI api `shouldBe`
                ruby "get_index_a_" ", a_: false" "get" "?#{\"&A!\" if a_}" "" "nil"
