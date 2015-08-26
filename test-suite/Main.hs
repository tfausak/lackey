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
        it "handles the simplest API" $ do
            let api = Proxy :: Proxy (Get '[] ())
            rubyForAPI api `shouldBe` "\
                \# @param http [Net::HTTP]\n\
                \# @return [Net::HTTPResponse]\n\
                \def get_index(http)\n\
                \  http.get('/')\n\
                \end\
            \"
