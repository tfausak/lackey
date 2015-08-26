{-# LANGUAGE DataKinds #-}

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
                \# @param http [Net::HTTP]\n\
                \# @return [Net::HTTPResponse]\n\
                \def delete_index(http)\n\
                \  http.delete('/')\n\
                \end\
            \"

        it "generates a function for getting the index" $ do
            let api = Proxy :: Proxy (Get '[] ())
            rubyForAPI api `shouldBe` "\
                \# @param http [Net::HTTP]\n\
                \# @return [Net::HTTPResponse]\n\
                \def get_index(http)\n\
                \  http.get('/')\n\
                \end\
            \"

        it "generates a function for posting the index" $ do
            let api = Proxy :: Proxy (Post '[] ())
            rubyForAPI api `shouldBe` "\
                \# @param http [Net::HTTP]\n\
                \# @return [Net::HTTPResponse]\n\
                \def post_index(http)\n\
                \  http.post('/', nil)\n\
                \end\
            \"
