{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import qualified Data.Proxy as Proxy
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
            Lackey.rubyForAPI api `shouldBe` "def get(excon)excon.request(:method=>:get,:path=>\"/\",:headers=>{},:body=>nil)end"
