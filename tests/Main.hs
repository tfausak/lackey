{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Data.Proxy
import Data.Text
import Lackey
import Servant.API
import Test.Hspec

main :: IO ()
main = hspec . parallel . describe "Lackey" . describe "rubyForAPI" $ do

  it "supports delete requests" $ do
    let api = Proxy :: Proxy (Delete '[JSON] ())
    rubyForAPI api `shouldBe`
      ruby "delete" "" "delete" "" "" False

  it "supports get requests" $ do
    let api = Proxy :: Proxy (Get '[JSON] ())
    rubyForAPI api `shouldBe`
      ruby "get" "" "get" "" "" False

  it "supports patch requests" $ do
    let api = Proxy :: Proxy (Patch '[JSON] ())
    rubyForAPI api `shouldBe`
      ruby "patch" "" "patch" "" "" False

  it "supports post requests" $ do
    let api = Proxy :: Proxy (Post '[JSON] ())
    rubyForAPI api `shouldBe`
      ruby "post" "" "post" "" "" False

  it "supports put requests" $ do
    let api = Proxy :: Proxy (Put '[JSON] ())
    rubyForAPI api `shouldBe`
      ruby "put" "" "put" "" "" False

  it "supports alternatives" $ do
    let api = Proxy :: Proxy (Get '[JSON] () :<|> Delete '[JSON] ())
    rubyForAPI api `shouldBe` mconcat
      [ ruby "get" "" "get" "" "" False
      , singleton ';'
      , ruby "delete" "" "delete" "" "" False
      ]

  it "supports captures" $ do
    let api = Proxy :: Proxy (Capture "id" () :> Get '[JSON] ())
    rubyForAPI api `shouldBe`
      ruby "get_by_id" ",id" "get" "#{id}" "" False

  it "supports query flags" $ do
    let api = Proxy :: Proxy (QueryFlag "flag" :> Get '[JSON] ())
    rubyForAPI api `shouldBe`
      ruby "get" ",flag: nil" "get" "?flag=#{flag}" "" False

  it "supports query params" $ do
    let api = Proxy :: Proxy (QueryParam "param" () :> Get '[JSON] ())
    rubyForAPI api `shouldBe`
      ruby "get" ",param: nil" "get" "?param=#{param}" "" False

  it "supports multiple query params" $ do
    let api = Proxy :: Proxy (QueryParams "params" () :> Get '[JSON] ())
    rubyForAPI api `shouldBe`
      ruby "get" ",params: nil" "get" "?params=#{params}" "" False

  it "supports request bodies" $ do
    let api = Proxy :: Proxy (ReqBody '[JSON] () :> Get '[JSON] ())
    rubyForAPI api `shouldBe`
      ruby "get" ",body" "get" "" "" True

  it "supports request headers" $ do
    let api = Proxy :: Proxy (Header "cookie" () :> Get '[JSON] ())
    rubyForAPI api `shouldBe`
      ruby "get" ",cookie: nil" "get" "" "\"cookie\"=>cookie" False

  it "supports response headers" $ do
    let api = Proxy :: Proxy (Get '[JSON] (Headers '[] ()))
    rubyForAPI api `shouldBe`
      ruby "get" "" "get" "" "" False

  it "puts the body param after captures" $ do
    let api = Proxy :: Proxy (Capture "segment" () :> ReqBody '[JSON] () :> Get '[JSON] ())
    rubyForAPI api `shouldBe`
      ruby "get_by_segment" ",segment,body" "get" "#{segment}" "" True

  it "puts the body param after query params" $ do
    let api = Proxy :: Proxy (QueryFlag "flag" :> ReqBody '[JSON] () :> Get '[JSON] ())
    rubyForAPI api `shouldBe`
      ruby "get" ",flag: nil,body" "get" "?flag=#{flag}" "" True

  it "sanitizes path segments" $ do
    let api = Proxy :: Proxy ("A!" :> Get '[JSON] ())
    rubyForAPI api `shouldBe`
      ruby "get_A!" "" "get" "A!" "" False

  it "sanitizes captures" $ do
    let api = Proxy :: Proxy (Capture "A!" () :> Get '[JSON] ())
    rubyForAPI api `shouldBe`
      ruby "get_by_A!" ",a_" "get" "#{a_}" "" False

  it "sanitizes query flags" $ do
    let api = Proxy :: Proxy (QueryFlag "A!" :> Get '[JSON] ())
    rubyForAPI api `shouldBe`
      ruby "get" ",a_: nil" "get" "?A!=#{a_}" "" False

  it "sanitizes query params" $ do
    let api = Proxy :: Proxy (QueryParam "A!" () :> Get '[JSON] ())
    rubyForAPI api `shouldBe`
      ruby "get" ",a_: nil" "get" "?A!=#{a_}" "" False

  it "sanitizes multiple query params" $ do
    let api = Proxy :: Proxy (QueryParams "A!" () :> Get '[JSON] ())
    rubyForAPI api `shouldBe`
      ruby "get" ",a_: nil" "get" "?A!=#{a_}" "" False

  it "sanitizes headers" $ do
    let api = Proxy :: Proxy (Header "A!" () :> Get '[JSON] ())
    rubyForAPI api `shouldBe`
      ruby "get" ",a_: nil" "get" "" "\"A!\"=>a_" False

-- Since every generated function has the same structure, it can be abstracted
-- away behind this function.
ruby :: String -> String -> String -> String -> String -> Bool -> Text
ruby name params method path headers body = pack (mconcat
  [ "def ", name, "(excon", params, ")"
  , "excon.request("
  , ":method=>:", method, ","
  , ":path=>\"/", path, "\","
  , ":headers=>{", headers, "},"
  , ":body=>", if body then "body" else "nil"
  , ")"
  , "end"
  ])
