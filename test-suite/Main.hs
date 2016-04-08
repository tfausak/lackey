{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import Data.Monoid ((<>))
import qualified Data.Proxy as Proxy
import qualified Data.Text as Text
import qualified Lackey
import qualified Servant.API as Servant
import qualified Test.Tasty as Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
    test <- testSpec "Lackey" spec
    Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
    describe "rubyForAPI" $ do
        it "supports delete requests" $ do
            let api = Proxy.Proxy :: Proxy.Proxy (Servant.Delete '[Servant.JSON] ())
            Lackey.rubyForAPI api `shouldBe`
                ruby "delete" "" "delete" "" "" False

        it "supports get requests" $ do
            let api = Proxy.Proxy :: Proxy.Proxy (Servant.Get '[Servant.JSON] ())
            Lackey.rubyForAPI api `shouldBe`
                ruby "get" "" "get" "" "" False

        it "supports patch requests" $ do
            let api = Proxy.Proxy :: Proxy.Proxy (Servant.Patch '[Servant.JSON] ())
            Lackey.rubyForAPI api `shouldBe`
                ruby "patch" "" "patch" "" "" False

        it "supports post requests" $ do
            let api = Proxy.Proxy :: Proxy.Proxy (Servant.Post '[Servant.JSON] ())
            Lackey.rubyForAPI api `shouldBe`
                ruby "post" "" "post" "" "" False

        it "supports put requests" $ do
            let api = Proxy.Proxy :: Proxy.Proxy (Servant.Put '[Servant.JSON] ())
            Lackey.rubyForAPI api `shouldBe`
                ruby "put" "" "put" "" "" False

        it "supports alternatives" $ do
            let api = Proxy.Proxy :: Proxy.Proxy (Servant.Get '[Servant.JSON] () Servant.:<|> Servant.Delete '[Servant.JSON] ())
            Lackey.rubyForAPI api `shouldBe` Text.concat
                [ ruby "get" "" "get" "" "" False
                , ";"
                , ruby "delete" "" "delete" "" "" False
                ]

        it "supports captures" $ do
            let api = Proxy.Proxy :: Proxy.Proxy (Servant.Capture "id" () Servant.:> Servant.Get '[Servant.JSON] ())
            Lackey.rubyForAPI api `shouldBe`
                ruby "get_by_id" ",id" "get" "#{id}" "" False

        it "supports query flags" $ do
            let api = Proxy.Proxy :: Proxy.Proxy (Servant.QueryFlag "flag" Servant.:> Servant.Get '[Servant.JSON] ())
            Lackey.rubyForAPI api `shouldBe`
                ruby "get" ",flag: nil" "get" "?flag=#{flag}" "" False

        it "supports query params" $ do
            let api = Proxy.Proxy :: Proxy.Proxy (Servant.QueryParam "param" () Servant.:> Servant.Get '[Servant.JSON] ())
            Lackey.rubyForAPI api `shouldBe`
                ruby "get" ",param: nil" "get" "?param=#{param}" "" False

        it "supports multiple query params" $ do
            let api = Proxy.Proxy :: Proxy.Proxy (Servant.QueryParams "params" () Servant.:> Servant.Get '[Servant.JSON] ())
            Lackey.rubyForAPI api `shouldBe`
                ruby "get" ",params: nil" "get" "?params=#{params}" "" False

        it "supports request bodies" $ do
            let api = Proxy.Proxy :: Proxy.Proxy (Servant.ReqBody '[Servant.JSON] () Servant.:> Servant.Get '[Servant.JSON] ())
            Lackey.rubyForAPI api `shouldBe`
                ruby "get" ",body" "get" "" "" True

        it "supports request headers" $ do
            let api = Proxy.Proxy :: Proxy.Proxy (Servant.Header "cookie" () Servant.:> Servant.Get '[Servant.JSON] ())
            Lackey.rubyForAPI api `shouldBe`
                ruby "get" ",cookie: nil" "get" "" "\"cookie\"=>cookie" False

        it "supports response headers" $ do
            let api = Proxy.Proxy :: Proxy.Proxy (Servant.Get '[Servant.JSON] (Servant.Headers '[] ()))
            Lackey.rubyForAPI api `shouldBe`
                ruby "get" "" "get" "" "" False

        it "puts the body param after captures" $ do
            let api = Proxy.Proxy :: Proxy.Proxy (Servant.Capture "segment" () Servant.:> Servant.ReqBody '[Servant.JSON] () Servant.:> Servant.Get '[Servant.JSON] ())
            Lackey.rubyForAPI api `shouldBe`
                ruby "get_by_segment" ",segment,body" "get" "#{segment}" "" True

        it "puts the body param after query params" $ do
            let api = Proxy.Proxy :: Proxy.Proxy (Servant.QueryFlag "flag" Servant.:> Servant.ReqBody '[Servant.JSON] () Servant.:> Servant.Get '[Servant.JSON] ())
            Lackey.rubyForAPI api `shouldBe`
                ruby "get" ",flag: nil,body" "get" "?flag=#{flag}" "" True

        it "sanitizes path segments" $ do
            let api = Proxy.Proxy :: Proxy.Proxy ("A!" Servant.:> Servant.Get '[Servant.JSON] ())
            Lackey.rubyForAPI api `shouldBe`
                ruby "get_A!" "" "get" "A!" "" False

        it "sanitizes captures" $ do
            let api = Proxy.Proxy :: Proxy.Proxy (Servant.Capture "A!" () Servant.:> Servant.Get '[Servant.JSON] ())
            Lackey.rubyForAPI api `shouldBe`
                ruby "get_by_A!" ",a_" "get" "#{a_}" "" False

        it "sanitizes query flags" $ do
            let api = Proxy.Proxy :: Proxy.Proxy (Servant.QueryFlag "A!" Servant.:> Servant.Get '[Servant.JSON] ())
            Lackey.rubyForAPI api `shouldBe`
                ruby "get" ",a_: nil" "get" "?A!=#{a_}" "" False

        it "sanitizes query params" $ do
            let api = Proxy.Proxy :: Proxy.Proxy (Servant.QueryParam "A!" () Servant.:> Servant.Get '[Servant.JSON] ())
            Lackey.rubyForAPI api `shouldBe`
                ruby "get" ",a_: nil" "get" "?A!=#{a_}" "" False

        it "sanitizes multiple query params" $ do
            let api = Proxy.Proxy :: Proxy.Proxy (Servant.QueryParams "A!" () Servant.:> Servant.Get '[Servant.JSON] ())
            Lackey.rubyForAPI api `shouldBe`
                ruby "get" ",a_: nil" "get" "?A!=#{a_}" "" False

        it "sanitizes headers" $ do
            let api = Proxy.Proxy :: Proxy.Proxy (Servant.Header "A!" () Servant.:> Servant.Get '[Servant.JSON] ())
            Lackey.rubyForAPI api `shouldBe`
                ruby "get" ",a_: nil" "get" "" "\"A!\"=>a_" False

-- Since every generated function has the same structure, it can be abstracted
-- away behind this function.
ruby :: Text.Text -> Text.Text -> Text.Text -> Text.Text -> Text.Text -> Bool -> Text.Text
ruby name params method path headers body = "\
    \def " <> name <> "(excon" <> params <> ")\
      \excon.request(\
        \:method=>:" <> method <> ",\
        \:path=>\"/" <> path <> "\",\
        \:headers=>{" <> headers <> "},\
        \:body=>" <> (if body then "body" else "nil") <> "\
      \)\
    \end\
\"
