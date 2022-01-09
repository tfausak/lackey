{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import qualified Data.Proxy as Proxy
import qualified Data.Text as Text
import qualified Lackey
import qualified Servant.API as Servant
import qualified Test.Hspec as Hspec

main :: IO ()
main =
  Hspec.hspec
    . Hspec.parallel
    . Hspec.describe "Lackey"
    . Hspec.describe "rubyForAPI"
    $ do

        Hspec.it "supports delete requests" $ do
          let
            api =
              Proxy.Proxy :: Proxy.Proxy (Servant.Delete '[Servant.JSON] ())
          Lackey.rubyForAPI api
            `Hspec.shouldBe` ruby "delete" "" "delete" "" "" False

        Hspec.it "supports get requests" $ do
          let
            api = Proxy.Proxy :: Proxy.Proxy (Servant.Get '[Servant.JSON] ())
          Lackey.rubyForAPI api
            `Hspec.shouldBe` ruby "get" "" "get" "" "" False

        Hspec.it "supports patch requests" $ do
          let
            api =
              Proxy.Proxy :: Proxy.Proxy (Servant.Patch '[Servant.JSON] ())
          Lackey.rubyForAPI api
            `Hspec.shouldBe` ruby "patch" "" "patch" "" "" False

        Hspec.it "supports post requests" $ do
          let
            api = Proxy.Proxy :: Proxy.Proxy (Servant.Post '[Servant.JSON] ())
          Lackey.rubyForAPI api
            `Hspec.shouldBe` ruby "post" "" "post" "" "" False

        Hspec.it "supports put requests" $ do
          let
            api = Proxy.Proxy :: Proxy.Proxy (Servant.Put '[Servant.JSON] ())
          Lackey.rubyForAPI api
            `Hspec.shouldBe` ruby "put" "" "put" "" "" False

        Hspec.it "supports alternatives" $ do
          let
            api =
              Proxy.Proxy :: Proxy.Proxy
                  ( Servant.Get '[Servant.JSON] () Servant.:<|> Servant.Delete '[Servant.JSON] ()
                  )
          Lackey.rubyForAPI api `Hspec.shouldBe` Text.concat
            [ ruby "get" "" "get" "" "" False
            , Text.singleton ';'
            , ruby "delete" "" "delete" "" "" False
            ]

        Hspec.it "supports captures" $ do
          let
            api =
              Proxy.Proxy :: Proxy.Proxy
                  ( Servant.Capture "id" () Servant.:> Servant.Get '[Servant.JSON] ()
                  )
          Lackey.rubyForAPI api
            `Hspec.shouldBe` ruby "get_by_id" ",id" "get" "#{id}" "" False

        Hspec.it "supports query flags" $ do
          let
            api =
              Proxy.Proxy :: Proxy.Proxy
                  ( Servant.QueryFlag "flag" Servant.:> Servant.Get '[Servant.JSON] ()
                  )
          Lackey.rubyForAPI api
            `Hspec.shouldBe` ruby
                               "get"
                               ",flag: nil"
                               "get"
                               "?flag=#{flag}"
                               ""
                               False

        Hspec.it "supports query params" $ do
          let
            api =
              Proxy.Proxy :: Proxy.Proxy
                  ( Servant.QueryParam "param" () Servant.:> Servant.Get '[Servant.JSON] ()
                  )
          Lackey.rubyForAPI api
            `Hspec.shouldBe` ruby
                               "get"
                               ",param: nil"
                               "get"
                               "?param=#{param}"
                               ""
                               False

        Hspec.it "supports multiple query params" $ do
          let
            api =
              Proxy.Proxy :: Proxy.Proxy
                  ( Servant.QueryParams "params" () Servant.:> Servant.Get '[Servant.JSON] ()
                  )
          Lackey.rubyForAPI api
            `Hspec.shouldBe` ruby
                               "get"
                               ",params: nil"
                               "get"
                               "?params=#{params}"
                               ""
                               False

        Hspec.it "supports request bodies" $ do
          let
            api =
              Proxy.Proxy :: Proxy.Proxy
                  ( Servant.ReqBody '[Servant.JSON] () Servant.:> Servant.Get '[Servant.JSON] ()
                  )
          Lackey.rubyForAPI api
            `Hspec.shouldBe` ruby "get" ",body" "get" "" "" True

        Hspec.it "supports request headers" $ do
          let
            api =
              Proxy.Proxy :: Proxy.Proxy
                  ( Servant.Header "cookie" () Servant.:> Servant.Get '[Servant.JSON] ()
                  )
          Lackey.rubyForAPI api
            `Hspec.shouldBe` ruby
                               "get"
                               ",cookie: nil"
                               "get"
                               ""
                               "\"cookie\"=>cookie"
                               False

        Hspec.it "supports response headers" $ do
          let
            api =
              Proxy.Proxy :: Proxy.Proxy
                  (Servant.Get '[Servant.JSON] (Servant.Headers '[] ()))
          Lackey.rubyForAPI api
            `Hspec.shouldBe` ruby "get" "" "get" "" "" False

        Hspec.it "puts the body param after captures" $ do
          let
            api =
              Proxy.Proxy :: Proxy.Proxy
                  ( Servant.Capture "segment" () Servant.:> Servant.ReqBody '[Servant.JSON] () Servant.:> Servant.Get '[Servant.JSON] ()
                  )
          Lackey.rubyForAPI api
            `Hspec.shouldBe` ruby
                               "get_by_segment"
                               ",segment,body"
                               "get"
                               "#{segment}"
                               ""
                               True

        Hspec.it "puts the body param after query params" $ do
          let
            api =
              Proxy.Proxy :: Proxy.Proxy
                  ( Servant.QueryFlag "flag" Servant.:> Servant.ReqBody '[Servant.JSON] () Servant.:> Servant.Get '[Servant.JSON] ()
                  )
          Lackey.rubyForAPI api
            `Hspec.shouldBe` ruby
                               "get"
                               ",flag: nil,body"
                               "get"
                               "?flag=#{flag}"
                               ""
                               True

        Hspec.it "sanitizes path segments" $ do
          let
            api =
              Proxy.Proxy :: Proxy.Proxy
                  ("A!" Servant.:> Servant.Get '[Servant.JSON] ())
          Lackey.rubyForAPI api
            `Hspec.shouldBe` ruby "get_A!" "" "get" "A!" "" False

        Hspec.it "sanitizes captures" $ do
          let
            api =
              Proxy.Proxy :: Proxy.Proxy
                  ( Servant.Capture "A!" () Servant.:> Servant.Get '[Servant.JSON] ()
                  )
          Lackey.rubyForAPI api
            `Hspec.shouldBe` ruby "get_by_A!" ",a_" "get" "#{a_}" "" False

        Hspec.it "sanitizes query flags" $ do
          let
            api =
              Proxy.Proxy :: Proxy.Proxy
                  ( Servant.QueryFlag "A!" Servant.:> Servant.Get '[Servant.JSON] ()
                  )
          Lackey.rubyForAPI api
            `Hspec.shouldBe` ruby "get" ",a_: nil" "get" "?A!=#{a_}" "" False

        Hspec.it "sanitizes query params" $ do
          let
            api =
              Proxy.Proxy :: Proxy.Proxy
                  ( Servant.QueryParam "A!" () Servant.:> Servant.Get '[Servant.JSON] ()
                  )
          Lackey.rubyForAPI api
            `Hspec.shouldBe` ruby "get" ",a_: nil" "get" "?A!=#{a_}" "" False

        Hspec.it "sanitizes multiple query params" $ do
          let
            api =
              Proxy.Proxy :: Proxy.Proxy
                  ( Servant.QueryParams "A!" () Servant.:> Servant.Get '[Servant.JSON] ()
                  )
          Lackey.rubyForAPI api
            `Hspec.shouldBe` ruby "get" ",a_: nil" "get" "?A!=#{a_}" "" False

        Hspec.it "sanitizes headers" $ do
          let
            api =
              Proxy.Proxy :: Proxy.Proxy
                  ( Servant.Header "A!" () Servant.:> Servant.Get '[Servant.JSON] ()
                  )
          Lackey.rubyForAPI api
            `Hspec.shouldBe` ruby "get" ",a_: nil" "get" "" "\"A!\"=>a_" False

-- Since every generated function has the same structure, it can be abstracted
-- away behind this function.
ruby :: String -> String -> String -> String -> String -> Bool -> Text.Text
ruby name params method path headers body = Text.pack
  (concat
    [ "def "
    , name
    , "(excon"
    , params
    , ")"
    , "excon.request("
    , ":method=>:"
    , method
    , ","
    , ":path=>\"/"
    , path
    , "\","
    , ":headers=>{"
    , headers
    , "},"
    , ":body=>"
    , if body then "body" else "nil"
    , ")"
    , "end"
    ]
  )
