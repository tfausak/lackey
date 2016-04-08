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
        it "supports get requests" $ do
            let api = Proxy.Proxy :: Proxy.Proxy (Servant.Get '[Servant.JSON] ())
            Lackey.rubyForAPI api `shouldBe`
                ruby "get" "" "get" "" "" False

-- Since every generated function has the same structure, it can be abstracted
-- away behind this function.
ruby :: Text.Text -> Text.Text -> Text.Text -> Text.Text -> Text.Text -> Bool -> Text.Text
ruby name params method path headers body = "\
    \def " <> name <> "(excon" <> params <> ")\
      \excon.request(\
        \:method=>:" <> method <> ",\
        \:path=>\"/" <> path <> "\",\
        \:headers=>{" <> headers <> "},\
        \:body=>" <> if body then "body" else "nil" <> "\
      \)\
    \end\
\"
