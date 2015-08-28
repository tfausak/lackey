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
        it "generates a function for deleting the index" $ do
            let api = Proxy :: Proxy (Delete '[] ())
            rubyForAPI api `shouldBe` "\
                \def delete_index(excon)\n\
                \  excon.delete(\"/\")\n\
                \end\
            \"

        it "generates a function for getting the index" $ do
            let api = Proxy :: Proxy (Get '[] ())
            rubyForAPI api `shouldBe` "\
                \def get_index(excon)\n\
                \  excon.get(\"/\")\n\
                \end\
            \"

        it "generates a function for posting the index" $ do
            let api = Proxy :: Proxy (Post '[] ())
            rubyForAPI api `shouldBe` "\
                \def post_index(excon)\n\
                \  excon.post(\"/\", nil)\n\
                \end\
            \"

        it "generates a function for getting a resource" $ do
            let api = Proxy :: Proxy ("resource" :> Get '[] ())
            rubyForAPI api `shouldBe` "\
                \def get_resource(excon)\n\
                \  excon.get(\"/resource\")\n\
                \end\
            \"

        it "generates a function for getting a nested resource" $ do
            let api = Proxy :: Proxy ("nested" :> "resource" :> Get '[] ())
            rubyForAPI api `shouldBe` "\
                \def get_nested_resource(excon)\n\
                \  excon.get(\"/nested/resource\")\n\
                \end\
            \"

        it "generates functions for two endpoints" $ do
            let api = Proxy :: Proxy (Get '[] () :<|> Delete '[] ())
            rubyForAPI api `shouldBe` "\
                \def get_index(excon)\n\
                \  excon.get(\"/\")\n\
                \end\n\
                \\n\
                \def delete_index(excon)\n\
                \  excon.delete(\"/\")\n\
                \end\
            \"
